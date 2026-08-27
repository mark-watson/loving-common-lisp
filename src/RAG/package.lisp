;;;; package.lisp — Package definition for Agentic RAG

(defpackage #:rag
  (:use #:cl)
  (:export #:make-corpus
           #:add-document
           #:save-corpus
           #:load-corpus
           #:corpus-chunk-count
           #:query
           #:agentic-rag
           #:interactive-demo
           #:test
           #:*rag-verbose*
           #:*rag-model*))

(defpackage #:rag-tests
  (:use #:cl)
  (:export #:run-tests))
