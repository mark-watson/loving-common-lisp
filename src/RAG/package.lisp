;;;; package.lisp — Package definition for Agentic RAG

(defpackage #:rag
  (:use #:cl)
  (:export #:make-corpus
           #:add-document
           #:query
           #:agentic-rag
           #:interactive-demo
           #:test))
