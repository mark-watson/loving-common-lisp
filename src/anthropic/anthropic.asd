;;;; anthropic.asd

(asdf:defsystem #:anthropic
  :description "Library for using the Anthropic LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "anthropic")))

