;;;; rag.asd — Agentic RAG system definition

(asdf:defsystem #:rag
  :description "Agentic RAG (Retrieval-Augmented Generation) using Gemini"
  :author "Mark Watson"
  :license "Apache 2"
  :version "1.0.0"
  :serial t
  :depends-on (#:llm #:cl-json #:uiop)
  :components ((:file "package")
               (:file "embeddings")
               (:file "vector-store")
               (:file "agents")
               (:file "rag")))
