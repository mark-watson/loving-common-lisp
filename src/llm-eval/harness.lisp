;;;; harness.lisp -- run a system over a golden dataset and score it.

(in-package #:llm-eval)

(defstruct eval-result
  case-id
  answer
  exact
  f1
  keywords
  judge          ; 0, 1, or NIL when not judged or the judge failed
  judge-reason
  seconds
  error)         ; error message string when the system failed

(defun elapsed-seconds (start)
  (float (/ (- (get-internal-real-time) start) internal-time-units-per-second)))

(defun run-eval (system cases &key (use-judge t) (verbose t))
  "Call SYSTEM, a function of one question string that returns an answer
string, on each of CASES and score each answer. An error in one case is
recorded in its result, and the run goes on to the next case."
  (loop for eval-case in cases
        for index from 1
        collect
        (let* ((start (get-internal-real-time))
               (error-message nil)
               (answer (handler-case (funcall system (eval-case-question eval-case))
                         (error (condition)
                           (setf error-message (princ-to-string condition))
                           "")))
               (seconds (elapsed-seconds start))
               (reference (eval-case-reference eval-case)))
          (multiple-value-bind (judge-score judge-reason)
              (cond (error-message (values 0 "System error"))
                    (use-judge (judge (eval-case-question eval-case) reference answer))
                    (t (values nil "")))
            (when verbose
              (format t "~&[~D/~D] ~A: judge ~A, ~,1F s~%"
                      index (length cases) (eval-case-id eval-case)
                      (or judge-score "-") seconds))
            (make-eval-result
             :case-id (eval-case-id eval-case)
             :answer answer
             :exact (exact-match answer reference)
             :f1 (float (token-f1 answer reference))
             :keywords (float (keyword-recall answer (eval-case-keywords eval-case)))
             :judge judge-score
             :judge-reason judge-reason
             :seconds seconds
             :error error-message)))))

(defun mean (numbers)
  (let ((numbers (remove nil numbers)))
    (if numbers
        (float (/ (reduce #'+ numbers) (length numbers)))
        nil)))

(defun summarize (results)
  "Return a plist of mean scores over RESULTS."
  (list :cases (length results)
        :errors (count-if #'eval-result-error results)
        :exact (mean (mapcar #'eval-result-exact results))
        :f1 (mean (mapcar #'eval-result-f1 results))
        :keywords (mean (mapcar #'eval-result-keywords results))
        :judge (mean (mapcar #'eval-result-judge results))
        :seconds (mean (mapcar #'eval-result-seconds results))))

(defun print-report (results &optional (stream *standard-output*))
  (format stream "~&~28A ~5A ~5A ~5A ~5A~%" "case" "exact" "f1" "keys" "judge")
  (dolist (result results)
    (format stream "~28A ~5D ~5,2F ~5,2F ~5A~@[  ERROR: ~A~]~%"
            (eval-result-case-id result)
            (eval-result-exact result)
            (eval-result-f1 result)
            (eval-result-keywords result)
            (or (eval-result-judge result) "-")
            (eval-result-error result)))
  (destructuring-bind (&key cases errors exact f1 keywords judge seconds)
      (summarize results)
    (format stream "~&~28A ~5,2F ~5,2F ~5,2F ~5,2F~%"
            "mean" exact f1 keywords (or judge 0))
    (format stream "~D cases, ~D errors, ~,1F seconds per case~%"
            cases errors seconds)))
