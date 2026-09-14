;;;; rag.asd — Agentic RAG system definition

(asdf:defsystem #:rag
  :description "Agentic RAG (Retrieval-Augmented Generation) using Gemini"
  :author "Mark Watson"
  :license "Apache 2"
  :version "1.1.0"
  :serial t
  :depends-on (#:cl-json #:dexador #:usocket #:uiop)
  :components ((:file "package")
               (:file "embeddings")
               (:file "vector-store")
               (:file "agents")
               (:file "rag"))
  :in-order-to ((asdf:test-op (asdf:test-op #:rag/test))))

(asdf:defsystem #:rag/test
  :description "Offline unit tests for the rag system (no network access)."
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:rag #:uiop)
  :serial t
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :rag-tests :run-tests)))
