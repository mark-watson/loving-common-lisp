;;;; package.lisp

(defpackage #:huggingface
  (:use #:cl #:uiop #:cl-json)
  (:export #:answer-question #:summarize))
