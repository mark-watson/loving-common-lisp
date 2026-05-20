;;;; package.lisp

(defpackage #:entities
  (:use #:cl #:myutils)
  (:export #:entity #:entities #:make-entities #:make-entities-object #:text->entities
           #:entities-cities #:entities-companies #:entities-countries
           #:entities-people #:entities-products #:entities-universities))

