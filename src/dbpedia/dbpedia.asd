;;;; dbpedia.asd

(asdf:defsystem #:dbpedia
  :description "Client for the DBpedia Lookup keyword search API"
  :author "Mark Watson"
  :license "AGPL-3.0 or Apache 2"
  :depends-on (#:drakma #:babel #:s-xml)
  :components ((:file "package")
               (:file "dbpedia")))

