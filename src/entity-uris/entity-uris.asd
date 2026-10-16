;;;; entity-uris.asd

(asdf:defsystem #:entity-uris
  :description "Find entity names in text and map them to DBpedia URIs"
  :author "Mark Watson"
  :license "AGPL-3.0"
  :depends-on (#:SPLIT-SEQUENCE :myutils)
  :serial t
  :components ((:file "package")
               (:file "entity-uris")))

