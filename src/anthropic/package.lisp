;;;; package.lisp

(defpackage #:anthropic
  (:use #:cl #:uiop #:cl-json)
  (:export #:completions))
