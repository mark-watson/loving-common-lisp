;;;; package.lisp

(defpackage #:tavily
  (:use :cl)
  (:import-from :babel :octets-to-string)
  (:export #:websearch))