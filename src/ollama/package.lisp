;;;; package.lisp

(defpackage #:ollama
  (:use #:cl #:uiop #:cl-json)
  (:export #:completions #:summarize #:answer-question #:embeddings #:dot-product))
