;;;; tests.lisp -- FiveAM unit tests. No test calls a real model.
;;;;
;;;;   (asdf:test-system :llm-eval)

(defpackage #:llm-eval-tests
  (:use #:cl #:llm-eval #:fiveam)
  (:export #:all-tests))

(in-package #:llm-eval-tests)

(def-suite all-tests :description "All llm-eval tests.")
(def-suite scoring :in all-tests)
(def-suite judging :in all-tests)
(def-suite datasets :in all-tests)
(def-suite harness :in all-tests)

;;; Scoring

(in-suite scoring)

(test tokens
  (is (equal '("hello" "world") (tokens "Hello, world!")))
  (is (equal '("1100" "per" "kwh") (tokens "$1,100 per kWh")))
  (is (equal '("1.1" "c") (tokens "1.1°C.")))
  (is (null (tokens "  ...  "))))

(test exact-match
  (is (= 1 (exact-match "About 73 miles." "about 73 miles")))
  (is (= 0 (exact-match "73 miles" "About 73 miles."))))

(test token-f1
  ;; 3 answer tokens, 6 reference tokens, 3 in common:
  ;; precision 1, recall 1/2, F1 2/3.
  (is (= 2/3 (token-f1 "about 280 ppm" "About 280 parts per million (ppm).")))
  (is (= 1 (token-f1 "Over 50%." "over 50")))
  (is (= 0 (token-f1 "no idea" "About 16%.")))
  (is (= 0 (token-f1 "" "About 16%."))))

(test keyword-recall
  (is (= 2/3 (keyword-recall "Toyota and QuantumScape lead." '("toyota" "samsung" "quantumscape"))))
  (is (= 1 (keyword-recall "It costs $250 to $600." '("250" "600"))))
  (is (= 0 (keyword-recall "1280 ppm" '("280"))) "Keywords match whole tokens only")
  (is (= 1 (keyword-recall "anything" '()))))

;;; Judging

(in-suite judging)

(test parse-judge-reply
  (is (equal '(1 "Same number.")
             (multiple-value-list (parse-judge-reply (format nil "SCORE: 1~%REASON: Same number.")))))
  (is (eql 0 (parse-judge-reply "score:0 reason: wrong")))
  (is (null (parse-judge-reply "I think the answer is fine.")))
  (is (null (parse-judge-reply "SCORE: 7"))))

(test judge-prompt-has-all-parts
  (let ((prompt (judge-prompt "Q?" "Ref." "Cand.")))
    (is (search "Question: Q?" prompt))
    (is (search "Reference answer: Ref." prompt))
    (is (search "Candidate answer: Cand." prompt))))

(test judge-with-fake-model
  (let ((*judge-function* (lambda (prompt)
                            (if (search "Candidate answer: 280" prompt)
                                (format nil "SCORE: 1~%REASON: match")
                                (format nil "SCORE: 0~%REASON: mismatch")))))
    (is (eql 1 (judge "q" "280 ppm" "280 ppm")))
    (is (eql 0 (judge "q" "280 ppm" "300 ppm"))))
  (let ((*judge-function* (lambda (prompt) (declare (ignore prompt)) "Looks good!")))
    (multiple-value-bind (score reason) (judge "q" "r" "a")
      (is (null score))
      (is (search "Unparsable" reason)))))

;;; Datasets

(in-suite datasets)

(test golden-dataset-loads
  (let ((cases (load-cases (asdf:system-relative-pathname :llm-eval "data/rag-golden.lisp"))))
    (is (= 12 (length cases)))
    (is (= (length cases)
           (length (remove-duplicates (mapcar #'eval-case-id cases) :test #'string=)))
        "Case ids must be unique")))

(test read-eval-is-off
  (uiop:with-temporary-file (:stream out :pathname path :direction :output)
    (write-string "(:id #.(error \"code ran\") :question \"q\" :reference \"r\")" out)
    :close-stream
    (signals reader-error (load-cases path))))

;;; Harness and store

(in-suite harness)

(defun fake-cases ()
  (list (make-eval-case :id "a" :question "qa" :reference "alpha" :keywords '("alpha"))
        (make-eval-case :id "b" :question "qb" :reference "beta" :keywords '("beta"))
        (make-eval-case :id "c" :question "qc" :reference "gamma" :keywords '("gamma"))))

(defun exact-judge (prompt)
  "A fake judge that passes an answer only if it repeats the reference."
  (let* ((reference (subseq prompt (+ (search "Reference answer: " prompt) 18)))
         (reference (subseq reference 0 (position #\Newline reference)))
         (candidate (subseq prompt (+ (search "Candidate answer: " prompt) 18)))
         (candidate (subseq candidate 0 (position #\Newline candidate))))
    (format nil "SCORE: ~D~%REASON: fake" (if (string= reference candidate) 1 0))))

(test run-eval-survives-a-failing-case
  (let* ((*judge-function* #'exact-judge)
         (results (run-eval (lambda (question)
                              (cond ((string= question "qa") "alpha")
                                    ((string= question "qb") (error "model crashed"))
                                    (t "wrong")))
                            (fake-cases) :verbose nil)))
    (is (equal '(1 0 0) (mapcar #'eval-result-judge results)))
    (is (search "model crashed" (eval-result-error (second results))))
    (is (= 1 (getf (summarize results) :errors)))))

(test store-finds-regressions
  (let ((*judge-function* #'exact-judge)
        (db (open-store ":memory:")))
    (unwind-protect
         (let* ((old (save-run db "old" '(:version 1)
                               (run-eval (lambda (q) (cond ((string= q "qa") "alpha")
                                                           ((string= q "qb") "beta")
                                                           (t "wrong")))
                                         (fake-cases) :verbose nil)))
                (new (save-run db "new" '(:version 2)
                               (run-eval (lambda (q) (cond ((string= q "qa") "alpha")
                                                           ((string= q "qc") "gamma")
                                                           (t "wrong")))
                                         (fake-cases) :verbose nil))))
           (multiple-value-bind (regressions improvements) (compare-runs db old new)
             (is (equal '(("b" 1.0d0 0.0d0)) regressions))
             (is (equal '(("c" 0.0d0 1.0d0)) improvements)))
           (is (equal '(nil nil)
                      (multiple-value-list (compare-runs db old new :threshold 1)))
               "Changes no larger than the threshold are ignored")
           (is (= 2 (length (run-summaries db)))))
      (sqlite:disconnect db))))

;;; The RAG pipeline with both model calls faked: the embedding model and
;;; the chat model. The test runs the real retrieval and agent code.

(defun fake-embedding (text)
  "A tiny bag-of-words embedding over a fixed vocabulary."
  (let ((words (tokens text)))
    (map 'simple-vector
         (lambda (word) (if (member word words :test #'string=) 1.0 0.0))
         '("co2" "ppm" "wind" "denmark" "battery"))))

(defun fake-chat (prompt &key model)
  (declare (ignore model))
  (cond ((search "Sufficient Context Agent" prompt)
         (format nil "VERDICT: SUFFICIENT~%REASON: ok~%MISSING: NONE"))
        ((search "Synthesis Agent" prompt)
         (if (search "280 ppm" prompt)
             "About 280 ppm (facts.txt)."
             "I don't know."))
        (t "pre-industrial co2 ppm")))

(test rag-pipeline-offline
  (uiop:with-temporary-file (:stream out :pathname path :direction :output :type "txt")
    (write-string "Pre-industrial CO2 was 280 ppm. Denmark gets half its power from wind." out)
    :close-stream
    (let ((rag:*rag-verbose* nil)
          (rag::*embedding-fn* #'fake-embedding)
          (rag::*embedding-cache* (make-hash-table :test 'equal))
          (rag::*generate-fn* #'fake-chat)
          (*judge-function* #'exact-judge))
      (let ((corpus (rag:make-corpus :name "test" :description "test")))
        (rag:add-document corpus path)
        (let ((results (run-eval (lambda (question) (rag:query corpus question))
                                 (list (make-eval-case :id "co2"
                                                       :question "Pre-industrial CO2?"
                                                       :reference "About 280 ppm (facts.txt)."
                                                       :keywords '("280")))
                                 :verbose nil)))
          (is (= 1 (eval-result-keywords (first results))))
          (is (eql 1 (eval-result-judge (first results)))))))))
