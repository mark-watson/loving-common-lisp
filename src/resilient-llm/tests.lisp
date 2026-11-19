;;;; tests.lisp -- offline tests. Fake models stand in for real ones, and
;;;; *SLEEP-FUNCTION* records delays instead of waiting.
;;;;
;;;;   (ql:quickload :resilient-llm/tests)
;;;;   (resilient-llm::run-tests)

(in-package #:resilient-llm)

(defvar *failures* 0)

(defmacro check (name form expected)
  `(let ((actual (handler-case ,form
                   (error (condition) (list :error (type-of condition))))))
     (if (equalp actual ,expected)
         (format t "~&pass  ~A~%" ,name)
         (progn (incf *failures*)
                (format t "~&FAIL  ~A~%      expected ~S~%      got      ~S~%"
                        ,name ,expected actual)))))

(defun rate-limit ()
  (error 'litelm:rate-limit-error :status 429 :body "slow down"))

(defun make-flaky-model (failures reply)
  "A fake model that signals a rate limit FAILURES times, then returns REPLY.
The second value of the result counts calls."
  (let ((calls 0))
    (values (lambda (model prompt)
              (declare (ignore model prompt))
              (incf calls)
              (if (<= calls failures) (rate-limit) reply))
            (lambda () calls))))

(defmacro with-fake-model ((function) &body body)
  `(let ((*completion-function* ,function)
         (*sleep-function* (lambda (seconds) (declare (ignore seconds)))))
     ,@body))

(defun run-tests ()
  (setf *failures* 0)

  ;; basics.lisp
  (check "skip bad ages"
         (parse-ages-skipping '("42" "abc" "7" "200"))
         '(42 7))
  (check "replace bad ages with a default"
         (parse-ages-with-default '("42" "abc" "7" "200") 0)
         '(42 0 7 0))
  (check "log, then skip"
         (with-output-to-string (*standard-output*)
           (parse-ages-logging '("1" "x")))
         (format nil "Log: Not a valid age: \"x\"~%"))
  (check "no handler: the error escapes"
         (parse-ages '("x"))
         '(:error bad-age))

  ;; retries
  (multiple-value-bind (model calls) (make-flaky-model 2 "ok")
    (let ((delays '()))
      (check "retry twice, then succeed"
             (with-fake-model (model)
               (handler-bind ((retrying (lambda (condition)
                                          (push (retrying-attempt condition) delays))))
                 (with-retries () (complete "hi"))))
             "ok")
      (check "three calls in total" (funcall calls) 3)
      (check "RETRYING signaled for attempts 1 and 2" (reverse delays) '(1 2))))

  (multiple-value-bind (model calls) (make-flaky-model 100 "never")
    (check "give up after max-retries"
           (with-fake-model (model)
             (with-retries (:max-retries 3) (complete "hi")))
           '(:error litelm:rate-limit-error))
    (check "one call plus three retries" (funcall calls) 4))

  (let ((calls 0))
    (check "no retry for errors that are not transient"
           (with-fake-model ((lambda (model prompt)
                               (declare (ignore model prompt))
                               (incf calls)
                               (error 'litelm:not-found-error :status 404 :body "no model")))
             (with-retries () (complete "hi")))
           '(:error litelm:not-found-error))
    (check "called once" calls 1))

  (check "a refused connection is transient"
         (transient-error-p (make-condition 'usocket:connection-refused-error :socket nil))
         t)
  (check "HTTP 503 is transient"
         (transient-error-p (make-condition 'litelm:api-error :status 503 :body ""))
         t)
  (check "HTTP 401 is not transient"
         (transient-error-p (make-condition 'litelm:authentication-error :status 401 :body ""))
         nil)

  (check "backoff stays between half the ceiling and the ceiling"
         (loop for attempt from 1 to 8
               for ceiling = (min 30.0 (expt 2 (1- attempt)))
               always (loop repeat 100
                            always (<= (/ ceiling 2)
                                       (backoff-delay attempt)
                                       ceiling)))
         t)

  ;; fallback
  (let ((models-called '()))
    (check "fall back to the next model"
           (with-fake-model ((lambda (model prompt)
                               (declare (ignore prompt))
                               (push model models-called)
                               (if (string= model "good")
                                   "hello from good"
                                   (error 'litelm:not-found-error :status 404 :body "no model"))))
             (with-fallback-models ("also-bad" "good")
               (with-retries ()
                 (complete "hi" :model "bad"))))
           "hello from good")
    (check "models tried in order" (reverse models-called) '("bad" "also-bad" "good")))

  (check "a handler can supply a canned reply with USE-VALUE"
         (with-fake-model ((lambda (model prompt)
                             (declare (ignore model prompt))
                             (rate-limit)))
           (handler-bind ((litelm:rate-limit-error
                            (lambda (condition) (use-value "canned reply" condition))))
             (complete "hi")))
         "canned reply")

  ;; JSON
  (check "strip a ```json fence"
         (strip-code-fences (format nil "```json~%{\"a\": 1}~%```"))
         "{\"a\": 1}")
  (check "parse a fenced reply"
         (with-fake-model ((lambda (model prompt)
                             (declare (ignore model prompt))
                             (format nil "```json~%{\"a\": 1}~%```")))
           (gethash "a" (complete-json "give me json")))
         1)

  (let ((prompts '()))
    (flet ((model (model prompt)
             (declare (ignore model))
             (push prompt prompts)
             (if (= (length prompts) 1) "{a: 1}" "{\"a\": 1}")))
      (check "no repair policy: MALFORMED-JSON escapes"
             (with-fake-model (#'model) (complete-json "give me json"))
             '(:error malformed-json))
      (setf prompts '())
      (check "repair malformed JSON"
             (with-fake-model (#'model)
               (with-json-repair ()
                 (gethash "a" (complete-json "give me json"))))
             1)
      (check "the repair prompt quotes the bad reply"
             (and (search "{a: 1}" (first prompts)) t)
             t)))

  (format t "~&~:[All tests passed~;~:*~D FAILED~].~%"
          (and (plusp *failures*) *failures*))
  (zerop *failures*))
