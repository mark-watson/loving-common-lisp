;;;; knowledge-base-navigator.asd

(asdf:defsystem #:knowledge-base-navigator
  :description "Knowledge Base Navigator using Gemini 3 Flash LLM"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:cl-json #:uiop #:alexandria)
  :serial t
  :components ((:file "project")
               (:file "knowledge-base-navigator")))
