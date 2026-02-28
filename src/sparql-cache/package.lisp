;;;; package.lisp

(defpackage #:sparql-cache
  ;(:use #:cl #:uiop #:cl-csv) ; Lispworks not loading cl-csv (required for graphdb)
  (:use #:cl #:uiop #:myutils #:sqlite)            ; Lispworks not loading cl-csv (required for graphdb)
  (:export #:sparql-manual #:dbpedia #:wikidata #:fuseki #:agraph #:stardog #:ask-dbpedia))
