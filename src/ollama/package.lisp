;;;; package.lisp

(defpackage #:ollama
  (:use #:cl)
  (:export #:completions
           #:completions-with-tools
           #:summarize
           #:answer-question
           #:register-tool-function
           #:cloud-search-agent
           #:*model-name*
           #:*tool-model-name*
           #:*model-host*
           #:*cloud-model-name*
           #:*cloud-host*))
