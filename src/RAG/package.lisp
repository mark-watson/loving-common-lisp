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
           #:*rag-model*
           #:*embedding-model*
           #:*embedding-dimension*
           #:*embedding-batch-limit*
           #:*embedding-cache-cap*
           #:clear-embedding-cache
           #:cosine-similarity
           #:dot-product
           #:vector-magnitude
           #:normalize-vector))

(defpackage #:rag-tests
  (:use #:cl)
  (:export #:run-tests))