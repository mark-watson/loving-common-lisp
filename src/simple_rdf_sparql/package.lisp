;;;; package.lisp

(defpackage #:simple_rdf_sparql
     (:use #:cl #:uiop)
     (:export #:add-triple #:remove-triple #:print-all-triples #:execute-sparql-query #:test))