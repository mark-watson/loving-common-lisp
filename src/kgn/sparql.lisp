(in-package #:kgn)

(ql:quickload "cl-json")
(ql:quickload "drakma")

(defun sparql-wikidata (query)
  (let* ((cr (fetch-result-wikidata query))
         (response
          (or
           cr
           (replace-all
            (replace-all
             (uiop:run-program 
              (list
               "curl" 
               (concatenate
                'string
                "https://query.wikidata.org/bigdata/namespace/wdq/sparql?query="
                (drakma:url-encode query :utf-8)
                "&format=json"))
              :output :string)
             "\\u2013" " ")
            "\\u" " "))))
    (save-query-result-wikidata query response)
    (with-input-from-string
	(s response)
      (let ((json-as-list (json:decode-json s)))
         ;;(pprint json-as-list)  ; uncomment this to see how following expression works:
         (mapcar #'(lambda (x)
                     (mapcar #'(lambda (y)
                                 (list (car y) (cdaddr y))) x))  (cdr (cadadr json-as-list)))))))

(defun sparql-dbpedia (query)
  ;;(print (list "\n\n" query "\n"))
  (let* (ret
         (cr (fetch-result-dbpedia query))
         (response
          (or
           cr
           (replace-all
            (replace-all
             (uiop:run-program 
              (list
               "curl" 
               (concatenate 'string
                            "https://dbpedia.org/sparql?query="
                            (drakma:url-encode query :utf-8)
                            "&format=json"))
              :output :string)
             "\\u2013" " ")
            "\\u" " "))))
    ;;(print (list "\n\n**** response:" response "\n\n"))
    (save-query-result-dbpedia query response)
    (ignore-errors
      (with-input-from-string
          (s response)
        (let ((json-as-list (json:decode-json s)))
          ;;(pprint json-as-list)  ; uncomment this to see how following expression works:
          (setf
           ret
           (mapcar #'(lambda (x)
                       ;;(pprint x)
                       (mapcar #'(lambda (y)
                                   (list (car y) (cdr (assoc :value (cdr y))))) x))
                   (cdr (cadddr (cadr json-as-list))))))))
    ret))


(defun sparql-ask-dbpedia (query)
  (let* ((cr (fetch-result-dbpedia query))
         (response
          (or
           cr
           (replace-all
            (replace-all
             (uiop:run-program 
              (list
               "curl" 
               (concatenate 'string
                            "https://dbpedia.org/sparql?query="
                            (drakma:url-encode query :utf-8)
                            "&format=json"))
              :output :string)
             "\\u2013" " ")
            "\\u" " "))))
    (save-query-result-dbpedia query response)
    (if  (search "true" response)
        t
      nil)))

#|
(setf dd (kgn::sparql-wikidata "select ?s ?p { ?s ?p \"Bill Gates\"@en }"))
(pprint dd)
(setf rr (kgn::sparql-dbpedia "select ?s ?p { ?s ?p \"Bill Gates\"@en }"))
(pprint rr)
|#