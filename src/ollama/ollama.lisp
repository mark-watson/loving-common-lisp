(in-package #:ollama)

;;; Basic Ollama completions without tool calling support
;;; For tool calling, see ollama-tools.lisp

(defvar *model-name* "qwen3-vl:2b")

(defun completions (starter-text &key (model *model-name*))
  "Simple completion without function/tool calling support."
  (let* ((full-model (ensure-model-name model))
         (resp (litelm:completion full-model
                                  :messages starter-text
                                  :api-base *model-host*)))
    (format t "Raw response: ~s~%" (litelm:response-raw resp))
    (or (litelm:response-content resp) "No response content")))

;;(ollama:completions "Complete the following text: The President went to")

;; Helper functions for summarization and question answering
(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))

(defun answer-question (some-text)
  (completions (concatenate 'string "
Q: " some-text "
A:")))

