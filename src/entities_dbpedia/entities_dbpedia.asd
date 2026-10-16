;;;; entities_dbpedia.asd

(asdf:defsystem #:entities_dbpedia
  :description "Find DBpedia entity names (and their URIs) in text"
  :author "Mark Watson"
  :license "AGPL-3.0 or Apache 2"
  :depends-on (#:SPLIT-SEQUENCE :myutils)
  :serial t
  :components ((:file "package")
               (:file "entities_dbpedia")))

