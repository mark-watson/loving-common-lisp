;;;; knowledgegraphnavigator.asd

(asdf:defsystem #:openai
  :description "Describe openai here"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:drakma #:cl-json)
  :components ((:file "package")
               (:file "openai")))

