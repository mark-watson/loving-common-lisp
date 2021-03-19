;;;; knowledgegraphnavigator.asd

(asdf:defsystem #:wolfram
  :description "Wolfram Language interface experiments"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:myutils)
  :components ((:file "package")
               (:file "wolfram")))

