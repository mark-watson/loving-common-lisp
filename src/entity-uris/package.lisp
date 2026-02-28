;;;; package.lisp

(defpackage #:entity-uris
  (:use #:cl #:myutils)
  (:export #:get-entity-names #:find-entities-in-text #:entity-iterator #:pp-entities #:ensure-uri-brackets))
