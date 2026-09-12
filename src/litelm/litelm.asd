;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License

(asdf:defsystem #:litelm
  :description "Minimal LLM routing + message translation across providers (gemini, fireworks-ai, ollama, deepseek, openai). Modelled after the Python litelm library, but messages and tool definitions use a Common Lisp friendly nested list format."
  :author "Mark Watson"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:dexador #:uiop)
  :components ((:file "package")
               (:file "json")
               (:file "providers")
               (:file "messages")
               (:file "litelm")))
