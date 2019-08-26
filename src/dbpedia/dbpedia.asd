;;;; dbpedia.asd

(asdf:defsystem #:dbpedia
  :description "Describe dbpedia here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma #:babel #:s-xml)
  :components ((:file "package")
               (:file "dbpedia")))

