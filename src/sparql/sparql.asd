;;;; knowledgegraphnavigator.asd

(asdf:defsystem #:sparql
  :description "SPARQL queries against DBpedia, Wikidata, and local RDF stores"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:drakma #:cl-json #:myutils)
  :components ((:file "package")
               (:file "sparql")))

  ;;:depends-on (#:uiop #:drakma #:cl-json #:cl-csv)  ; Lispworks not loading cl-csv (required for graphdb)

