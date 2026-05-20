;;;; gemini.asd

(asdf:defsystem #:gemini
  :description "Library for using the Google Gemini Interactions API"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:alexandria #:dexador)
  :components ((:file "package")
               (:file "gemini")
	       (:file "gemini_interactions_api")))
