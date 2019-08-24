;; Copyright Mark Watson 2001-2013. All Rights Reserved.  http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.

(ql:quickload :drakma)
(ql:quickload :cl-json)

(defun do-search (&rest terms)
  (let ((query-string (format nil "~{~A~^+AND+~}" terms)))
   (cl-json:decode-json-from-string
     (drakma:http-request
       (concatenate 
        'string
        "http://localhost:8983/solr/select?q="
        query-string
        "&wt=json")))))

;; Note: wrapping calls like:
;;   (cdr (cadddr (cadr (do-search "British" "one"))))
;; retunrs result documents a a simple list of documents without
;; Solr meta data.

   
(defun keys-values-to-xml-string  (keys-values-list)
 (with-output-to-string (stream)
   (format stream "<add><doc>")
   (dolist (kv keys-values-list)
     (format stream "<field name=\"")
     (format stream (car kv))
     (format stream "\">")
     (format stream (cdr kv))
     (format stream "\"</field>"))
   (format stream "</doc></add>")))

;; test:
;; (keys-values-to-xml-string '(("id" . "12345")("title" . "test title")))


;; add a new document to index.
;; note: one key/value must have key of "id".
;; if document with "id" already in index then
;; this overwrites key-values if they were defined already.
(defun do-add (keys-values-list)
  (drakma:http-request
   "http://localhost:8983/solr/update"
   :method :post
   :content-type "application/xml"
   :content ( keys-values-to-xml-string  keys-values-list)))

;; test:
;; (do-add '(("id" . "12345")("title" . "Fishing Season")))

(defun commit-adds ()
  (drakma:http-request
   "http://localhost:8983/solr/update"
   :method :post
   :content-type "application/xml"
   :content "<commit></commit>"))






