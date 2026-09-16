(in-package #:ollama)

(defvar *model-host* "http://localhost:11434/v1")

(defun ensure-model-name (model)
  "Ensure MODEL has a provider prefix for litelm routing (defaults to ollama/)."
  (if (find #\/ model)
      model
      (concatenate 'string "ollama/" model)))

