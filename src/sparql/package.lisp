;;;; package.lisp

(defpackage #:sparql
  ;(:use #:cl #:uiop #:cl-csv) ; Lispworks not loading cl-csv (required for graphdb)
  (:use #:cl #:uiop)            ; Lispworks not loading cl-csv (required for graphdb)
  (:export #:sparql-manual #:dbpedia #:wikidata #:fuseki #:agraph #:stardog #:ask-dbpedia))
