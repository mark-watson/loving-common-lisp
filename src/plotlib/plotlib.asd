;;;; dbpedia.asd

(asdf:defsystem #:plotlib
  :description "Describe plotlib here"
  :author "mark.watson@gmail.com"
  :license "Apache 2"
  :depends-on (#:vecto)
  :components ((:file "package")
               (:file "plotlib")))

