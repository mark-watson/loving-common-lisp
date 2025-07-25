;;;; package.lisp

(defpackage #:perplexity
  (:use #:cl #:uiop #:cl-json #:drakma)
  (:shadow "PARAMETER-ERROR")
  (:export #:research))
