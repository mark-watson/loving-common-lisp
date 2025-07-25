;;;; knowledgegraphnavigator.asd

;; one time only: create empty cache database: sqlite3 ~/.kgn_cache.db

(asdf:defsystem #:sparql-cache
  :description "Describe dbpedia here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:uiop #:drakma #:sqlite #:cl-json #:myutils)
  :components ((:file "package")
               (:file "sparql")))

  ;;:depends-on (#:uiop #:drakma #:cl-json #:cl-csv)  ; Lispworks not loading cl-csv (required for graphdb)

