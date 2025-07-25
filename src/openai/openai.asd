;;;; openai.asd

(asdf:defsystem #:openai
  :description "Library for using the beta OpenAI APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:drakma)
  :components ((:file "package")
               (:file "openai")
               (:file "utils")
	             (:file "groq"))) 

