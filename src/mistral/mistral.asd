;;;; mistral.asd

(asdf:defsystem #:mistral
  :description "Library for using the Mistral LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "mistral")))

