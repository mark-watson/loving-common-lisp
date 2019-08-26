;;;; spacy.lisp

(in-package spacy)

(defvar base-url "http://127.0.0.1:8008?text=")

(defstruct spacy-data entities tokens)
;;  (entities :type list)
;;  (tokens :type list))
  
(defun spacy-client (query)
  (let* ((the-bytes
	  (drakma:http-request
	   (concatenate 'string
			base-url
			(do-urlencode:urlencode  query))
	   :content-type "application/text"))
	 (fetched-data
	  (flexi-streams:octets-to-string the-bytes :external-format :utf-8))
	 (lists (with-input-from-string (s fetched-data)
		  (json:decode-json s))))
    (print lists)
    (make-spacy-data :entities (cadar lists) :tokens (cdadr lists))))
