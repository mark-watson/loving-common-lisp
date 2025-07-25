;;;; package.lisp

(defpackage #:entities_dbpedia
  (:use #:cl #:myutils)
  (:export #:get-entity-names #:find-entities-in-text #:entity-iterator))
