(in-package #:ollama)

;;; Basic Ollama completions without tool calling support
;;; For tool calling, see ollama-tools.lisp

(defvar *model-name* "mistral:v0.3")

(defun completions (starter-text)
  "Simple completion without function/tool calling support."
  (let* ((message (list (cons :|role| "user")
                        (cons :|content| starter-text)))
         (data (list (cons :|model| *model-name*)
                     (cons :|stream| nil)
                     (cons :|messages| (list message))))
         (json-data (lisp-to-json-string data))
         ;; Hack: cl-json encodes nil as null, but we need false for stream
         (fixed-json-data (substitute-subseq json-data ":null" ":false" :test #'string=))
         (curl-command
          (format nil "curl ~a -d ~s"
                  ollama::*model-host*
                  fixed-json-data)))
    (multiple-value-bind (content function-call)
        (ollama-helper curl-command)
      (declare (ignore function-call))
      (or content "No response content"))))

;;(ollama:completions "Complete the following text: The President went to")

;; Helper functions for summarization and question answering
(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))

(defun answer-question (some-text)
  (completions (concatenate 'string "
Q: " some-text "
A:")))
