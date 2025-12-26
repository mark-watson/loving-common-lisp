;;;; groq.asd

(asdf:defsystem #:groq
  :description "Library for using the beta Groq LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "groq"))) 

