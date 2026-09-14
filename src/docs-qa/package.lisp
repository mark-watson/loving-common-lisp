;;;; package.lisp

(defpackage #:docs-qa
  (:use #:cl #:uiop #:cl-json #:sqlite #:split-sequence)
  (:export #:QA))
