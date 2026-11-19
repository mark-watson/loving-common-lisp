;;;; resilient-llm.lisp -- restarts for LLM calls, and handler policies
;;;; that pick among them: retry with backoff, fall back to another model,
;;;; and ask the model to repair malformed JSON.

(in-package #:resilient-llm)

(defvar *default-model* "ollama/llama3.2:3b")

(defun litelm-completion (model prompt)
  (litelm:response-content (litelm:completion model :messages prompt)))

(defvar *completion-function* 'litelm-completion
  "Function of (MODEL PROMPT) that returns the reply text. Tests bind this
to a fake model.")

(defvar *sleep-function* 'sleep
  "Function of one argument, the number of seconds to wait. Tests bind
this to a function that only records the delay.")

;;; The low-level call. It decides nothing about failures; it only offers
;;; ways to recover.

(defun complete (prompt &key (model *default-model*))
  "Send PROMPT to MODEL and return the reply text. If the call fails, these
restarts are available: RETRY, USE-MODEL, and USE-VALUE."
  (loop
    (restart-case (return (funcall *completion-function* model prompt))
      (retry ()
        :report (lambda (stream) (format stream "Call ~A again." model)))
      (use-model (new-model)
        :report "Call a different model."
        :interactive (lambda ()
                       (format *query-io* "Model (for example \"ollama/qwen3.5:2b\"): ")
                       (finish-output *query-io*)
                       (list (read *query-io*)))
        (setf model new-model))
      (use-value (reply)
        :report "Return a reply of your choice instead."
        :interactive (lambda ()
                       (format *query-io* "Reply string: ")
                       (finish-output *query-io*)
                       (list (read *query-io*)))
        (return reply)))))

;;; Which errors are worth retrying?

(defun transient-error-p (condition)
  "True for errors that may go away if we wait: rate limits, server errors
\(HTTP 5xx), and network failures such as a refused connection."
  (or (typep condition 'litelm:rate-limit-error)
      (and (typep condition 'litelm:api-error)
           (integerp (litelm:api-error-status condition))
           (>= (litelm:api-error-status condition) 500))
      (typep condition 'usocket:socket-error)))

(define-condition retrying (condition)
  ((attempt :initarg :attempt :reader retrying-attempt)
   (delay :initarg :delay :reader retrying-delay)
   (cause :initarg :cause :reader retrying-cause))
  (:report (lambda (condition stream)
             (format stream "Retry ~D in ~,2F seconds after: ~A"
                     (retrying-attempt condition)
                     (retrying-delay condition)
                     (retrying-cause condition)))))

;; SBCL starts every process with the same *RANDOM-STATE*, so without
;; this seed every client would pick the same "random" delays.
(defvar *backoff-random-state* (make-random-state t))

(defun backoff-delay (attempt &key (base-delay 1.0) (max-delay 30.0))
  "Exponential backoff with jitter. The ceiling doubles with each ATTEMPT
\(1, 2, 3, ...) up to MAX-DELAY, and the delay is a random value from half
the ceiling to the full ceiling. The randomness keeps many clients that
failed at the same moment from retrying at the same moment."
  (let ((ceiling (min max-delay (* base-delay (expt 2 (1- attempt))))))
    (+ (/ ceiling 2) (random (/ ceiling 2.0) *backoff-random-state*))))

(defun call-with-retries (thunk &key (max-retries 3) (base-delay 1.0) (max-delay 30.0))
  "Call THUNK. When a transient error reaches us and a RETRY restart is
available, wait and invoke RETRY, at most MAX-RETRIES times in total for
the whole call. Other errors pass through untouched."
  (let ((retries 0))
    (handler-bind
        ((error (lambda (condition)
                  (let ((restart (find-restart 'retry condition)))
                    (when (and restart
                               (transient-error-p condition)
                               (< retries max-retries))
                      (incf retries)
                      (let ((delay (backoff-delay retries :base-delay base-delay
                                                          :max-delay max-delay)))
                        ;; SIGNAL returns NIL when no one handles RETRYING,
                        ;; so this costs nothing unless a caller listens.
                        (signal 'retrying :attempt retries :delay delay
                                          :cause condition)
                        (funcall *sleep-function* delay)
                        (invoke-restart restart)))))))
      (funcall thunk))))

(defmacro with-retries ((&rest options &key max-retries base-delay max-delay)
                        &body body)
  "Run BODY, retrying transient LLM errors with exponential backoff."
  (declare (ignore max-retries base-delay max-delay))
  `(call-with-retries (lambda () ,@body) ,@options))

(defun call-with-fallback-models (models thunk)
  "Call THUNK. When an LLM error reaches us, switch to the next model in
MODELS by invoking the USE-MODEL restart."
  (let ((remaining models))
    (handler-bind
        ((error (lambda (condition)
                  (let ((restart (find-restart 'use-model condition)))
                    (when (and restart
                               remaining
                               (or (typep condition 'litelm:litelm-error)
                                   (transient-error-p condition)))
                      (invoke-restart restart (pop remaining)))))))
      (funcall thunk))))

(defmacro with-fallback-models ((&rest models) &body body)
  "Run BODY. If a model call fails, try the next of MODELS."
  `(call-with-fallback-models (list ,@models) (lambda () ,@body)))

;;; JSON replies

(define-condition malformed-json (error)
  ((text :initarg :text :reader malformed-json-text)
   (cause :initarg :cause :reader malformed-json-cause))
  (:report (lambda (condition stream)
             (format stream "Model reply is not valid JSON (~A): ~S"
                     (malformed-json-cause condition)
                     (malformed-json-text condition)))))

(defun strip-code-fences (text)
  "Remove a Markdown code fence such as ```json ... ``` around TEXT. Small
models add these even when told not to."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (start (and (>= (length trimmed) 3)
                     (string= "```" trimmed :end2 3)
                     (position #\Newline trimmed)))
         (end (and start (search "```" trimmed :from-end t))))
    (if (and start end (> end start))
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (subseq trimmed (1+ start) end))
        trimmed)))

(defun parse-json-reply (text)
  "Parse TEXT as JSON, or signal MALFORMED-JSON."
  (handler-case (jzon:parse (strip-code-fences text))
    (error (condition)
      (error 'malformed-json :text text :cause condition))))

(defun repair-prompt (text condition)
  (format nil "This text should be valid JSON, but parsing failed with: ~A~%~%~A~%~%~
Reply with only the corrected JSON, no explanation and no Markdown."
          (malformed-json-cause condition) text))

(defun complete-json (prompt &key (model *default-model*))
  "Ask MODEL for JSON and return it parsed with jzon: objects become hash
tables and arrays become vectors. If the reply is not valid JSON, these
restarts are available: REPAIR-JSON and USE-VALUE."
  (let ((text (complete prompt :model model)))
    (loop
      (restart-case (return (parse-json-reply text))
        (repair-json (condition)
          :report "Ask the model to fix its JSON."
          (setf text (complete (repair-prompt text condition) :model model)))
        (use-value (value)
          :report "Return a value of your choice instead."
          :interactive (lambda ()
                         (format *query-io* "Value: ")
                         (finish-output *query-io*)
                         (list (read *query-io*)))
          (return value))))))

(defmacro with-json-repair ((&key (max-repairs 2)) &body body)
  "Run BODY. When a model reply is not valid JSON, ask the model to repair
it, at most MAX-REPAIRS times in total."
  (let ((repairs (gensym "REPAIRS")))
    `(let ((,repairs 0))
       (handler-bind
           ((malformed-json
              (lambda (condition)
                (let ((restart (find-restart 'repair-json condition)))
                  (when (and restart (< ,repairs ,max-repairs))
                    (incf ,repairs)
                    (invoke-restart restart condition))))))
         ,@body))))
