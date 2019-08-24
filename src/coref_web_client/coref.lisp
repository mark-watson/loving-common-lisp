;;;; coref.lisp

(in-package #:coref)

;; (ql:quickload :do-urlencode)

(defvar base-url "http://127.0.0.1:8000?text=")

(defun coref-client (query)
  (let ((the-bytes
	 (drakma:http-request
	  (concatenate 'string
		       base-url
		       (do-urlencode:urlencode  query)
		       "&no_detail=1")
	  :content-type "application/text")))
    (flexi-streams:octets-to-string the-bytes :external-format :utf-8)))
     
;; secret sauce: get the file path of this file:
;; (useful in NLP, etc. when we want to find data files at runtime)
;;

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
