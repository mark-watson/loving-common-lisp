;;;; package.lisp

(defpackage #:openai
  (:use #:cl #:uiop #:cl-json #:drakma)
  (:shadow "PARAMETER-ERROR")
  (:export #:completions #:summarize #:answer-question #:embeddings #:dot-product #:groq-completion #:groq-extract-content))
