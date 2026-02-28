;;;; entities_dbpedia.asd

(asdf:defsystem #:entities_dbpedia
  :description "Describe webscrape here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:SPLIT-SEQUENCE :myutils)
  :serial t
  :components ((:file "package")
               (:file "entities_dbpedia")))

