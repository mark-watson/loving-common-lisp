;;;; huggingface.asd

(asdf:defsystem #:huggingface
  :description "Library for using the beta huggingface APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "huggingface")))

