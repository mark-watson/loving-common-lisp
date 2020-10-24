;;;; package.lisp

(defpackage #:sparql
  (:use #:cl #:uiop #:cl-csv)
  (:export #:dbpedia #:wikidata #:fuseki #:agraph))

