;;;; package.lisp

(defpackage #:conceptnet
  ;(:use #:cl #:uiop #:cl-csv) ; Lispworks not loading cl-csv (required for graphdb)
  (:use #:cl #:uiop)            ; Lispworks not loading cl-csv (required for graphdb)
  (:export #:conceptnet))
