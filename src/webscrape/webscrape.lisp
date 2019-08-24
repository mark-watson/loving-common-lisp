;;;; webscrape.lisp

(in-package #:webscrape)

(defun fetch-page (uri)
  (drakma:http-request uri))

;;
;; secret sauce: get the file path of this file:
;; (useful in NLP, etc. when we want to find data files at runtime)
;;

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))

