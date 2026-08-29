;;; tests.lisp — Offline unit tests for the RAG system
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; These tests require no network access: *embedding-fn*,
;;; *batch-request-fn*, and *generate-fn* are rebound to deterministic
;;; stubs. Run with:
;;;   (asdf:test-system :rag)

(in-package #:rag-tests)

(defvar *failures* 0)

(defmacro check (form)
  "Evaluate FORM, recording (not signaling) a failure."
  `(handler-case
       (unless ,form
         (incf *failures*)
         (format t "~%FAIL: ~S~%" ',form))
     (error (e)
       (incf *failures*)
       (format t "~%ERROR in ~S: ~A~%" ',form e))))

(defmacro check-equal (expected form &key (test ''equal))
  `(let ((expected-value ,expected)
         (actual-value ,form))
     (unless (funcall ,test expected-value actual-value)
       (incf *failures*)
       (format t "~%FAIL: ~S~%  expected: ~S~%  actual:   ~S~%"
               ',form expected-value actual-value))))

(defmacro check-error (form)
  "Check that FORM signals an error."
  `(handler-case
       (progn ,form
              (incf *failures*)
              (format t "~%FAIL: ~S did not signal an error~%" ',form))
     (error () t)))

;;; ---- Chunking ----

(defun test-split-into-chunks ()
  ;; Empty text
  (check-equal nil (rag::split-into-chunks ""))
  ;; Text shorter than chunk-size -> single chunk
  (check-equal '("Short text.")
               (rag::split-into-chunks "Short text." :chunk-size 500))
  ;; Prose with sentence boundaries: chunks non-empty and bounded in size
  (let* ((sentence "The quick brown fox jumps over the lazy dog. ")
         (text (with-output-to-string (s) (dotimes (i 40) (write-string sentence s))))
         (chunks (rag::split-into-chunks text :chunk-size 100 :overlap 10)))
    (check (> (length chunks) 3))
    (check (every (lambda (c) (> (length c) 0)) chunks))
    (check (every (lambda (c) (<= (length c) 101)) chunks)))
  ;; Regression (B1/B2): text with a sentence break only at position 0 and
  ;; no later breaks. The old loop moved START backwards (never terminating
  ;; or erroring); the guard must guarantee strict forward progress.
  (let* ((text (concatenate 'string "." (make-string 300 :initial-element #\x)))
         (chunks (rag::split-into-chunks text :chunk-size 30 :overlap 10)))
    (check (> (length chunks) 5))
    (check (every (lambda (c) (> (length c) 0)) chunks))
    ;; Coverage: the final chunk must contain the end of the text
    (check (search (make-string 10 :initial-element #\x)
                   (car (last chunks)))))
  ;; Chunks must appear in document order
  (let* ((text "AAAA. BBBB. CCCC. DDDD. EEEE. FFFF. GGGG. HHHH.")
         (chunks (rag::split-into-chunks text :chunk-size 15 :overlap 3)))
    (check (search "AAAA" (first chunks)))))

;;; ---- Query line parsing (regression: digit-trimming bug) ----

(defun test-parse-query-lines ()
  ;; Plain queries pass through untouched, including digits
  (check-equal '("2024 lithium battery prices")
               (rag::parse-query-lines "2024 lithium battery prices"))
  (check-equal '("1.5 MW turbine output")
               (rag::parse-query-lines "1.5 MW turbine output"))
  (check-equal '("75-100 kg CO2 per kWh")
               (rag::parse-query-lines "75-100 kg CO2 per kWh"))
  (check-equal '("CO2 emissions 2024 targets")
               (rag::parse-query-lines "CO2 emissions 2024 targets"))
  ;; Numbered list prefixes are stripped for ANY number, not just 1-3
  (check-equal '("fourth query" "fifth query")
               (rag::parse-query-lines "4. fourth query
5. fifth query"))
  ;; Bullet and dash prefixes are stripped
  (check-equal '("a query" "another query")
               (rag::parse-query-lines "- a query
* another query"))
  ;; "1.5" with no space after the dot is query text, not a list prefix
  (check-equal '("1.5 MW output") (rag::parse-query-lines "1.5 MW output"))
  ;; Empty lines dropped; empty/nil response handled
  (check-equal '("only one")
               (rag::parse-query-lines "

only one

"))
  (check-equal nil (rag::parse-query-lines nil))
  (check-equal nil (rag::parse-query-lines "")))

;;; ---- Vector math ----

(defun test-vector-math ()
  (check-equal 32 (rag::dot-product '(1 2 3) '(4 5 6)))
  (check-equal 5.0 (rag::vector-magnitude '(3 4)))
  (check (= 1.0 (rag::cosine-similarity '(1 0 0) '(1 0 0))))
  (check (< (abs (rag::cosine-similarity '(1 0 0) '(0 1 0))) 1e-6))
  (check (= 0.0 (rag::cosine-similarity '(0 0 0) '(1 1 1))))
  ;; Regression: mismatched dimensions must signal, not truncate
  (check-error (rag::dot-product '(1 2 3) '(4 5 6 7 8)))
  (check-error (rag::cosine-similarity '(1 0 0) '(1 0 0 0 0)))
  ;; Works with vectors as well as lists
  (check-equal 32 (rag::dot-product #(1 2 3) #(4 5 6)))
  ;; normalize-vector: unit output, zero-safe, idempotent
  (let ((normed (rag::normalize-vector '(3 4))))
    (check (< (abs (- 1.0 (rag::vector-magnitude normed))) 1e-9))
    (check (< (abs (- (rag::dot-product '(3 4) normed) 5.0)) 1e-9)))
  (check-equal #(0.0 0.0 0.0) (rag::normalize-vector #(0 0 0))
               :test #'equalp)
  ;; cosine-similarity of normalized vectors equals the dot product
  ;; (single-float rounding allows ~1e-6, not 1e-9)
  (let ((a (rag::normalize-vector '(1 2 3)))
        (b (rag::normalize-vector '(2 1 1))))
    (check (< (abs (- (rag::cosine-similarity a b)
                      (rag::dot-product a b)))
              1.0e-6))
    ;; normalization is idempotent within single-float precision
    (check (equalp a (rag::normalize-vector a)))))

;;; ---- Retrieval ----

(defun make-test-chunk (text embedding &optional (source "test.txt"))
  (rag::make-document-chunk/embedded text source embedding))

(defun make-test-corpus ()
  (let ((corpus (rag:make-corpus :name "test" :description "test corpus")))
    (setf (rag::corpus-chunks corpus)
          (list (make-test-chunk "alpha" '(1.0 0.0 0.0))
                (make-test-chunk "beta"  '(0.9 0.1 0.0))
                (make-test-chunk "gamma" '(0.0 1.0 0.0))
                (make-test-chunk "delta" '(0.0 0.0 1.0))))
    corpus))

(defun test-search-corpus ()
  (let ((corpus (make-test-corpus)))
    ;; Ranking: most similar first
    (let ((results (rag::search-corpus corpus '(1.0 0.0 0.0) :top-k 2)))
      (check-equal 2 (length results))
      (check-equal "alpha" (rag::document-chunk-text (cdr (first results))))
      (check-equal "beta"  (rag::document-chunk-text (cdr (second results))))
      (check (> (car (first results)) (car (second results)))))
    ;; top-k larger than the corpus is clamped
    (check-equal 4 (length (rag::search-corpus corpus '(1.0 0.0 0.0) :top-k 99))))
  ;; Raw (un-normalized) query embeddings get the same scores as
  ;; normalized ones: search must normalize by the query norm
  (let ((corpus (make-test-corpus)))
    (check (< (abs (- (car (first (rag::search-corpus corpus '(2.0 0.0 0.0) :top-k 1)))
                      (car (first (rag::search-corpus corpus '(1.0 0.0 0.0) :top-k 1)))))
              1e-9))))

(defun test-search-corpora ()
  (let* ((corpus-a (make-test-corpus))
         (corpus-b (rag:make-corpus :name "test-b" :description "second")))
    (setf (rag::corpus-chunks corpus-b)
          (list (make-test-chunk "best" '(0.99 0.01 0.0) "b.txt")))
    (let ((results (rag::search-corpora (list corpus-a corpus-b)
                                        '(1.0 0.0 0.0) :top-k 2)))
      (check-equal 2 (length results))
      ;; Cross-corpus re-sort: alpha (1.0) still beats best (~0.99)
      (check-equal "alpha" (rag::document-chunk-text (cdr (first results))))
      (check-equal "best"  (rag::document-chunk-text (cdr (second results)))))))

(defun test-search-fanout-dedup ()
  ;; Two sub-queries with identical embeddings retrieve the same chunks;
  ;; fanout must deduplicate by (source . text).
  (let ((corpus (make-test-corpus))
        (rag::*embedding-fn* (lambda (text) (declare (ignore text)) '(1.0 0.0 0.0)))
        (rag::*embedding-cache* (make-hash-table :test 'equal))
        (rag::*rag-verbose* nil))
    (let ((results (rag::search-fanout (list corpus) '("q1" "q2") :top-k 4)))
      (check-equal 4 (length results))
      (check-equal 4 (length (remove-duplicates
                              results :test #'equal
                              :key (lambda (r) (rag::document-chunk-text (cdr r))))))))
  ;; Same text in different source files is NOT deduplicated
  (let* ((chunk-a (make-test-chunk "same text" '(1.0 0.0 0.0) "a.txt"))
         (chunk-b (make-test-chunk "same text" '(1.0 0.0 0.0) "b.txt"))
         (corpus (rag:make-corpus :name "two" :description "sources")))
    (setf (rag::corpus-chunks corpus) (list chunk-a chunk-b))
    (let ((rag::*embedding-fn* (lambda (text) (declare (ignore text)) '(1.0 0.0 0.0)))
          (rag::*embedding-cache* (make-hash-table :test 'equal))
          (rag::*rag-verbose* nil))
      (let ((results (rag::search-fanout (list corpus) '("q") :top-k 5)))
        (check-equal 2 (length results))))))

(defun test-search-fanout-batches-queries ()
  ;; search-fanout must embed all sub-queries in one batch call
  ;; (get-embeddings), not one call per query. *batch-request-fn* is
  ;; stubbed (not *embedding-fn*) so get-embeddings takes its batch path.
  (let ((corpus (make-test-corpus))
        (batch-calls 0)
        (rag::*rag-verbose* nil))
    (let ((rag::*batch-request-fn*
            (lambda (texts)
              (incf batch-calls)
              (mapcar (lambda (t%) (declare (ignore t%)) '(1.0 0.0 0.0))
                      texts)))
          (rag::*embedding-cache* (make-hash-table :test 'equal)))
      (let ((results (rag::search-fanout (list corpus)
                                         '("q1" "q2" "q3" "q4")
                                         :top-k 4)))
        (check-equal 4 (length results))
        (check-equal 1 batch-calls)))))

;;; ---- Embedding batching ----

(defun test-batch-splitting ()
  ;; More than *embedding-batch-limit* texts must split into multiple
  ;; batch requests, preserving order. All texts are cache misses, so
  ;; only *batch-request-fn* is ever called.
  (let ((sizes nil)
        (all-texts (loop for i from 1 to 205
                         collect (format nil "text ~A" i)))
        (rag::*embedding-cache* (make-hash-table :test 'equal))
        (rag::*rag-verbose* nil))
    (let ((rag::*batch-request-fn*
            (lambda (texts)
              (push (length texts) sizes)
              (mapcar (lambda (t%) (declare (ignore t%)) '(1.0)) texts))))
      (let ((results (rag::get-embeddings all-texts)))
        (check-equal 205 (length results))
        ;; batches of 100, 100, 5 in call order
        (check-equal '(100 100 5) (nreverse sizes))
        ;; every text now cached; a second call makes no HTTP at all
        (let ((second-sizes nil))
          (let ((rag::*batch-request-fn*
                  (lambda (texts) (push (length texts) second-sizes)
                    (mapcar (lambda (t%) (declare (ignore t%)) '(1.0))
                            texts))))
            (rag::get-embeddings all-texts)
            (check-equal nil second-sizes)))))))

(defun test-cache-eviction ()
  ;; Cache clears itself at the cap
  (let ((rag::*embedding-cache* (make-hash-table :test 'equal))
        (rag::*embedding-cache-cap* 3)
        (rag::*embedding-fn* (lambda (text) (declare (ignore text)) '(1.0)))
        (rag::*rag-verbose* nil))
    (dolist (text '("a" "b" "c" "d"))
      (rag::get-embedding text))
    (check-equal 1 (hash-table-count rag::*embedding-cache*))
    (check (nth-value 1 (gethash (rag::embedding-cache-key "d")
                                 rag::*embedding-cache*)))
    ;; "a" was evicted with the rest of the old cache
    (check (not (nth-value 1 (gethash (rag::embedding-cache-key "a")
                                      rag::*embedding-cache*))))))

;;; ---- Retry logic ----

(defun test-call-with-retries ()
  ;; Backoff sleep is stubbed so the suite stays fast.
  (let ((attempts 0)
        (rag::*rag-verbose* nil)
        (rag::*retry-sleep-fn* (lambda (s) (declare (ignore s)) nil)))
    ;; transient 503 twice, then success
    (setf attempts 0)
    (check-equal :success
                 (rag::call-with-retries
                  (lambda ()
                    (incf attempts)
                    (if (< attempts 3)
                        (error 'dex:http-request-failed
                               :status 503 :body "down")
                        :success))))
    (check-equal 3 attempts)
    ;; permanent 400 signals immediately, no retries
    (setf attempts 0)
    (check-error (rag::call-with-retries
                  (lambda ()
                    (incf attempts)
                    (error 'dex:http-request-failed
                           :status 400 :body "bad"))))
    (check-equal 1 attempts)
    ;; 429 is transient
    (setf attempts 0)
    (check-equal :ok
                 (rag::call-with-retries
                  (lambda ()
                    (incf attempts)
                    (if (= attempts 1)
                        (error 'dex:http-request-failed
                               :status 429 :body "slow")
                        :ok))))
    (check-equal 2 attempts)
    ;; usocket connection errors are transient
    (setf attempts 0)
    (check-equal :ok
                 (rag::call-with-retries
                  (lambda ()
                    (incf attempts)
                    (if (= attempts 1)
                        (error 'usocket:connection-refused-error)
                        :ok))))
    (check-equal 2 attempts)
    ;; exhausted retries signal with the underlying condition
    (setf attempts 0)
    (check-error (rag::call-with-retries
                  (lambda ()
                    (incf attempts)
                    (error 'dex:http-request-failed
                           :status 503 :body "down"))
                  :attempts 2))
    (check-equal 2 attempts)
    ;; attempts=1 means no retry at all
    (setf attempts 0)
    (check-error (rag::call-with-retries
                  (lambda ()
                    (incf attempts)
                    (error 'usocket:connection-refused-error))
                  :attempts 1))
    (check-equal 1 attempts)))

;;; ---- Sufficiency verdict parsing (regression: B4) ----

(defun test-parse-verdict ()
  (multiple-value-bind (ok feedback)
      (rag::parse-verdict-response
       "VERDICT: SUFFICIENT
REASON: all facts present
MISSING: NONE")
    (check ok)
    (check-equal "NONE" feedback))
  (multiple-value-bind (ok feedback)
      (rag::parse-verdict-response
       "VERDICT: INSUFFICIENT
REASON: no pricing data
MISSING: current lithium-ion battery prices")
    (check (not ok))
    (check-equal "current lithium-ion battery prices" feedback))
  ;; Case-insensitive
  (check (nth-value 0 (rag::parse-verdict-response
                       "verdict: sufficient")))
  ;; Unparseable verdict -> documented fallback is SUFFICIENT (bounds cost)
  (check (nth-value 0 (let ((rag::*rag-verbose* nil))
                        (rag::parse-verdict-response "I am not sure."))))
  ;; NIL/empty response must not crash
  (check (nth-value 0 (let ((rag::*rag-verbose* nil))
                        (rag::parse-verdict-response nil)))))

;;; ---- Corpus persistence ----

(defun test-corpus-persistence ()
  (let ((path (merge-pathnames "rag-test-corpus.sexp"
                               (uiop:temporary-directory))))
    (unwind-protect
         (let* ((corpus (make-test-corpus))
                (loaded (progn (rag:save-corpus corpus path)
                               (rag:load-corpus path))))
           (check-equal "test" (rag::corpus-name loaded))
           (check-equal (rag:corpus-chunk-count corpus)
                        (rag:corpus-chunk-count loaded))
           (check-equal (mapcar #'rag::document-chunk-text (rag::corpus-chunks corpus))
                        (mapcar #'rag::document-chunk-text (rag::corpus-chunks loaded)))
           ;; embeddings round-trip exactly: chunks are stored normalized
           ;; and load-corpus re-normalization is a no-op
           (check (equalp (mapcar #'rag::document-chunk-embedding
                                  (rag::corpus-chunks corpus))
                          (mapcar #'rag::document-chunk-embedding
                                  (rag::corpus-chunks loaded))))
           ;; round trip keeps search working
           (let ((results (rag::search-corpus loaded '(1.0 0.0 0.0) :top-k 1)))
             (check-equal "alpha"
                          (rag::document-chunk-text (cdr (first results))))))
      (ignore-errors (delete-file path)))))

(defun test-load-corpus-validation ()
  ;; corrupt files must signal, not produce NIL embeddings
  (let ((path (merge-pathnames "rag-bad-corpus.sexp"
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; missing embedding
           (with-open-file (out path :direction :output
                                     :if-exists :supersede)
             (prin1 (list :name "bad"
                          :chunks (list (list :text "t" :source "s")))
                    out))
           (check-error (rag:load-corpus path))
           ;; truncated structure
           (with-open-file (out path :direction :output
                                     :if-exists :supersede)
             (prin1 (list :name "bad") out))
           (check-error (rag:load-corpus path))
           ;; garbage
           (with-open-file (out path :direction :output
                                     :if-exists :supersede)
             (prin1 42 out))
           (check-error (rag:load-corpus path)))
      (ignore-errors (delete-file path)))))

;;; ---- Full pipeline with stubbed LLM and embeddings ----

(defun test-agentic-rag-pipeline ()
  (let ((corpus (make-test-corpus))
        (assess-calls 0)
        (generate-calls 0)
        (rag::*embedding-fn* (lambda (text) (declare (ignore text)) '(1.0 0.0 0.0)))
        (rag::*embedding-cache* (make-hash-table :test 'equal))
        (rag::*rag-verbose* nil))
    ;; Sufficient on the first assessment
    (let ((rag::*generate-fn*
            (lambda (prompt &key model)
              (declare (ignore model))
              (incf generate-calls)
              (cond ((search "Sufficient Context Agent" prompt)
                     (incf assess-calls)
                     "VERDICT: SUFFICIENT
REASON: context covers it
MISSING: NONE")
                    ((search "Synthesis Agent" prompt) "THE ANSWER")
                    (t "sub-query one")))))
      (check-equal "THE ANSWER"
                   (rag::agentic-rag (list corpus) "test question"))
      (check-equal 1 assess-calls))
    ;; Insufficient once, then sufficient: refine loop must run
    (setf assess-calls 0)
    (let ((rag::*generate-fn*
            (lambda (prompt &key model)
              (declare (ignore model))
              (cond ((search "Sufficient Context Agent" prompt)
                     (incf assess-calls)
                     (if (= assess-calls 1)
                         "VERDICT: INSUFFICIENT
REASON: missing facts
MISSING: more facts"
                         "VERDICT: SUFFICIENT
REASON: now complete
MISSING: NONE"))
                    ((search "Synthesis Agent" prompt) "REFINED ANSWER")
                    (t "refined query")))))
      (check-equal "REFINED ANSWER"
                   (rag::agentic-rag (list corpus) "test question"))
      (check-equal 2 assess-calls))
    ;; max-context-chunks caps the passages sent to the LLM
    (check-equal 2 (length (rag::%cap-context
                            (list (cons 0.9 :a) (cons 0.8 :b) (cons 0.7 :c)) 2)))
    ;; Verbose off means no pipeline banner or iteration headers
    (setf assess-calls 0)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((rag::*generate-fn*
                            (lambda (prompt &key model)
                              (declare (ignore model))
                              (cond ((search "Sufficient Context Agent" prompt)
                                     (incf assess-calls)
                                     "VERDICT: SUFFICIENT
REASON: ok
MISSING: NONE")
                                    ((search "Synthesis Agent" prompt) "A")
                                    (t "q")))))
                      (rag::agentic-rag (list corpus) "quiet question")))))
      (check (zerop (length output))))
    ;; Insufficient all the way: last iteration must skip the sufficiency
    ;; call (it cannot change the outcome), so with max-iterations 3 only
    ;; 2 assess calls happen.
    (setf assess-calls 0)
    (let ((rag::*generate-fn*
            (lambda (prompt &key model)
              (declare (ignore model))
              (cond ((search "Sufficient Context Agent" prompt)
                     (incf assess-calls)
                     "VERDICT: INSUFFICIENT
REASON: never enough
MISSING: more")
                    ((search "Synthesis Agent" prompt) "BEST EFFORT")
                    (t "refined")))))
      (check-equal "BEST EFFORT"
                   (rag::agentic-rag (list corpus) "test question"
                                     :max-iterations 3))
      (check-equal 2 assess-calls))
    ;; rewrite-queries always includes the original query: the fanout
    ;; prompt for the original must appear via the sub-query list
    (setf assess-calls 0)
    (let ((rag::*generate-fn*
            (lambda (prompt &key model)
              (declare (ignore model))
              (cond ((search "Sufficient Context Agent" prompt)
                     (incf assess-calls)
                     "VERDICT: SUFFICIENT
REASON: ok
MISSING: NONE")
                    ((search "Synthesis Agent" prompt) "A")
                    (t "rewritten sub-query")))))
      (rag::agentic-rag (list corpus) "original user question")
      (check-equal 1 assess-calls))
    ;; direct check: rewrite-queries appends the original
    (let ((rag::*generate-fn*
            (lambda (prompt &key model)
              (declare (ignore prompt model))
              "generated query")))
      (check-equal '("generated query" "orig")
                   (rag::rewrite-queries "orig"))))
  ;; rag-generate retries transient failures through *generate-fn*
  (let ((attempts 0)
        (rag::*rag-verbose* nil)
        (rag::*retry-sleep-fn* (lambda (s) (declare (ignore s)) nil)))
    (let ((rag::*generate-fn*
            (lambda (prompt &key model)
              (declare (ignore prompt model))
              (incf attempts)
              (if (= attempts 1)
                  (error 'dex:http-request-failed
                         :status 500 :body "oops")
                  "recovered text"))))
      (check-equal "recovered text" (rag::rag-generate "p")))))

;;; ---- Runner ----

(defun run-tests ()
  "Run all offline tests. Signals an error if any test fails."
  (setf *failures* 0)
  (mapc #'funcall
        '(test-split-into-chunks
          test-parse-query-lines
          test-vector-math
          test-search-corpus
          test-search-corpora
          test-search-fanout-dedup
          test-search-fanout-batches-queries
          test-batch-splitting
          test-cache-eviction
          test-call-with-retries
          test-parse-verdict
          test-corpus-persistence
          test-load-corpus-validation
          test-agentic-rag-pipeline))
  (if (zerop *failures*)
      (format t "~%~%All RAG tests passed.~%")
      (error "~A RAG test(s) failed." *failures*))
  t)