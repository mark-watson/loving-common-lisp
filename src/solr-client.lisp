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