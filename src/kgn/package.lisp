;;;; package.lisp

(defpackage #:kgn
  (:use #:cl #:alexandria #:myutils #:sqlite #:myutils
   #:lw-grapher #:trivial-open-browser #:entities #:entity-uris
   #:kbnlp #:CAPI)
  (:export #:kgn))
