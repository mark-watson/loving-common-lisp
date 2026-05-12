;;; cl-ai-coding-agent.asd -- ASDF system definition
(in-package #:asdf-user)

(defsystem "cl-ai-coding-agent"
  :name "cl-ai-coding-agent"
  :version "0.1.0"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :description
  "An AI coding agent that reads directories and files,
   writes new files, and diagnoses stacktraces."
  :depends-on ("gemini" "uiop")
  :components ((:file "package")
               (:file "tools")
               (:file "agent")))
