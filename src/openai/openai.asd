;;;; openai.asd

(asdf:defsystem #:openai
  :description "Library for using the beta OpenAPI APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "openai")))

