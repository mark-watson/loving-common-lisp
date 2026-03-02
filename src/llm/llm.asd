(asdf:defsystem #:llm
  :description "A generic interface for LLMs (Gemini, OpenAI, Ollama) with tool support"
  :author "Mark Watson"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:cl-json)
  :components ((:file "llm")
               (:file "simple-tools")
               (:file "gemini")
               (:file "openai")
               (:file "claude")
               (:file "ollama")))
