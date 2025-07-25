;;;; package.lisp

(defpackage #:myutils
  (:use #:cl)
  (:export #:replace-all #:tokenize-string #:tokenize-string-keep-uri #:words-from-string #:file-to-string #:node-name-from-uri))

