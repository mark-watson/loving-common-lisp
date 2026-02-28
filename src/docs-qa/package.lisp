;;;; package.lisp

(defpackage #:docs-qa
  (:use #:cl #:uiop #:cl-json #:openai #:sqlite #:split-sequence)
  (:export #:QA))
