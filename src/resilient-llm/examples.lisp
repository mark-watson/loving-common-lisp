;;;; examples.lisp -- live examples. They need a local Ollama server with
;;;; the model llama3.2:3b (ollama pull llama3.2:3b).
;;;;
;;;;   (ql:quickload :resilient-llm)
;;;;   (load "examples.lisp")

(in-package #:resilient-llm)

;; A provider on a port where nothing listens, so every call to it fails
;; with a refused connection.
(litelm:define-provider :ollama-down "http://localhost:11999/v1" :requires-key nil)

(defun log-retries (condition)
  (format t "~&~A~%" condition))

(defun example-fallback ()
  (format t "~&--- Missing model, fall back to llama3.2:3b~%")
  (with-fallback-models ("ollama/llama3.2:3b")
    (with-retries ()
      (complete "In one sentence, what is Common Lisp?"
                :model "ollama/no-such-model"))))

(defun example-retries-then-fallback ()
  (format t "~&--- Server down: retry with backoff, then fall back~%")
  (handler-bind ((retrying #'log-retries))
    (with-fallback-models ("ollama/llama3.2:3b")
      (with-retries (:max-retries 3 :base-delay 0.5)
        (complete "Name one Common Lisp implementation. Reply with only its name."
                  :model "ollama-down/llama3.2:3b")))))

(defun example-json ()
  (format t "~&--- JSON reply~%")
  (let ((result
          (with-json-repair ()
            ;; This handler runs first and returns normally, so it only
            ;; logs; WITH-JSON-REPAIR then picks the REPAIR-JSON restart.
            (handler-bind ((malformed-json
                             (lambda (condition)
                               (format t "~&Repairing: ~A~%" condition))))
              (complete-json
               "Return a JSON object with keys \"name\" and \"year\" for the
programming language Common Lisp, where year is the year ANSI approved
the standard. Reply with only the JSON.")))))
    (format t "name: ~A, year: ~A~%"
            (gethash "name" result) (gethash "year" result))
    result))

(format t "~A~%~%" (example-fallback))
(format t "~A~%~%" (example-retries-then-fallback))
(example-json)
