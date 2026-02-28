;;;; package.lisp

(defpackage #:ollama
  (:use #:cl #:uiop #:cl-json)
  (:export #:completions #:completions-with-tools #:summarize
           #:answer-question
           *model-name* *tool-model-name* *model-host*))
