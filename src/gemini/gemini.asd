;;;; gemini.asd

(asdf:defsystem #:gemini
  :description "Library for using the perplexity search+LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:dexador #:alexandria) ; Added fiveam
  :components ((:file "package")
               (:file "gemini")))