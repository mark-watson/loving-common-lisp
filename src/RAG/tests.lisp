;;; tests.lisp — Offline unit tests for the RAG system
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; These tests require no network access: *embedding-fn* and
;;; *generate-fn* are rebound to deterministic stubs. Run with:
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

;;; ---- Vector math ----

(defun test-vector-math ()
  (check-equal 32 (rag::dot-product '(1 2 3) '(4 5 6)))
  (check-equal 5.0 (rag::vector-magnitude '(3 4)))
  (check (= 1.0 (rag::cosine-similarity '(1 0 0) '(1 0 0))))
  (check (< (abs (rag::cosine-similarity '(1 0 0) '(0 1 0))) 1e-6))
  (check (= 0.0 (rag::cosine-similarity '(0 0 0) '(1 1 1)))))

;;; ---- Retrieval ----

(defun make-test-chunk (text embedding &optional (source "test.txt"))
  (rag::make-document-chunk :text text :source source :embedding embedding))

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
    (check-equal 4 (length (rag::search-corpus corpus '(1.0 0.0 0.0) :top-k 99)))))

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
  ;; fanout must deduplicate by chunk text.
  (let ((corpus (make-test-corpus))
        (rag::*embedding-fn* (lambda (text) (declare (ignore text)) '(1.0 0.0 0.0)))
        (rag::*embedding-cache* (make-hash-table :test 'equal))
        (rag::*rag-verbose* nil))
    (let ((results (rag::search-fanout (list corpus) '("q1" "q2") :top-k 4)))
      (check-equal 4 (length results))
      (check-equal 4 (length (remove-duplicates
                              results :test #'equal
                              :key (lambda (r) (rag::document-chunk-text (cdr r)))))))))

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
           (check (equalp (mapcar #'rag::document-chunk-embedding (rag::corpus-chunks corpus))
                          (mapcar #'rag::document-chunk-embedding (rag::corpus-chunks loaded)))))
      (ignore-errors (delete-file path)))))

;;; ---- Full pipeline with stubbed LLM and embeddings ----

(defun test-agentic-rag-pipeline ()
  (let ((corpus (make-test-corpus))
        (assess-calls 0)
        (rag::*embedding-fn* (lambda (text) (declare (ignore text)) '(1.0 0.0 0.0)))
        (rag::*embedding-cache* (make-hash-table :test 'equal))
        (rag::*rag-verbose* nil))
    ;; Sufficient on the first assessment
    (let ((rag::*generate-fn*
            (lambda (prompt &key model)
              (declare (ignore model))
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
                            (list (cons 0.9 :a) (cons 0.8 :b) (cons 0.7 :c)) 2)))))

;;; ---- Runner ----

(defun run-tests ()
  "Run all offline tests. Signals an error if any test fails."
  (setf *failures* 0)
  (mapc #'funcall
        '(test-split-into-chunks
          test-vector-math
          test-search-corpus
          test-search-corpora
          test-search-fanout-dedup
          test-parse-verdict
          test-corpus-persistence
          test-agentic-rag-pipeline))
  (if (zerop *failures*)
      (format t "~%~%All RAG tests passed.~%")
      (error "~A RAG test(s) failed." *failures*))
  t)
