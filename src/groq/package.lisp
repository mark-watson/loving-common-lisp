;;;; package.lisp

(defpackage #:groq
  (:use #:cl #:uiop #:cl-json)
  (:shadow "PARAMETER-ERROR")
  (:export #:groq-completion #:groq-extract-content))
