;;;; knowledgegraphnavigator.asd

;; one time only: create empty cache database: sqlite3 ~/.kgn_cache.db

(asdf:defsystem #:sparql-cache
  :description "SPARQL queries against DBpedia and Wikidata with a SQLite result cache"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:drakma #:sqlite #:cl-json #:myutils)
  :components ((:file "package")
               (:file "sparql")))

  ;;:depends-on (#:uiop #:drakma #:cl-json #:cl-csv)  ; Lispworks not loading cl-csv (required for graphdb)

