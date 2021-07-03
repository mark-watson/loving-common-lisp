;;;; package.lisp

(defpackage #:openai
  (:use #:cl #:uiop #:cl-json)
  (:export #:completions #:summarize #:answer-question))
