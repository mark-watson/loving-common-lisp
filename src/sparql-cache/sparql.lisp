;; Compyright 2021-2022 Mark Watson All Rights Reserved.
;; License: Apache 2

(in-package #:sparql-cache)

;; caching code:

;; one time only: create empty cache database: sqlite3 ~/.kgn_cache.db

(ql:quickload "sqlite")

;;; command line arguments:

(defun command-line-args ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

;;; SqList caching for SPARQL queries:

;;(defvar *db-path* (pathname "~/.kgn_cache.db"))
(defvar *db-path* (pathname "kgn_cache.db"))

(defun create-dbpedia ()
  (sqlite:with-open-database (d *db-path*)
    (ignore-errors
      (sqlite:execute-single d "CREATE TABLE dbpedia (query string  PRIMARY KEY ASC, result string)"))))

(defun save-query-result-dbpedia (query result)
  (sqlite:with-open-database (d *db-path*)
    (ignore-errors
      (sqlite:execute-to-list d
                       "insert into dbpedia (query, result) values (?, ?)"
                       query result))))
(defun fetch-result-dbpedia (query)
  (sqlite:with-open-database (d *db-path*)
    (cadar
     (sqlite:execute-to-list d
                      "select * from dbpedia where query = ?" query))))
(create-dbpedia)

(defun create-wikidata ()
  (sqlite:with-open-database (d *db-path*)
    (ignore-errors
      (sqlite:execute-single d "CREATE TABLE wikidata (query string  PRIMARY KEY ASC, result string)"))))

(defun save-query-result-wikidata (query result)
  (sqlite:with-open-database (d *db-path*)
    (ignore-errors
      (sqlite:execute-to-list d
                       "insert into wikidata (query, result) values (?, ?)"
                       query result))))
(defun fetch-result-wikidata (query)
  (sqlite:with-open-database (d *db-path*)
    (cadar
     (sqlite:execute-to-list d
                      "select * from wikidata where query = ?" query))))
(create-wikidata)

;;; SPARQL query code:

(ql:quickload "cl-json")
(ql:quickload "drakma")

(defun wikidata (query)
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

(defun dbpedia (query)
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


(defun ask-dbpedia (query)
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
(defun sparql-manual ()
  (princ "Package sparql examples:")
  (terpri) (terpri)
  (princ "
(sparql-cache:dbpedia \"select * { ?s ?p ?o } limit 4\")
(sparql-cache:wikidata \"select ?s ?p { ?s ?p ?o } limit 4\")
")
  (terpri)
  "")

#|
(setf dd (sparql-cache:wikidata "select ?s ?p { ?s ?p \"Bill Gates\"@en }"))
(pprint dd)
(setf rr (sparql-cache:dbpedia "select ?s ?p { ?s ?p \"Steve Jobs\"@en }"))
(pprint rr)

 (sparql-cache::fuseki "select ?s ?p ?o { ?s ?p ?o }")

 (sparql-cache::graphdb "KBS" "select ?s ?p ?o { ?s ?p ?o } limit 20")

 (sparql-cache::agraph "select ?s ?p ?o { ?s ?p ?o }")

 (sparql-cache::stardog "select ?s ?p ?o { ?s ?p ?o } limit 7")

|#

;; curl "http://127.0.0.1:3030/news/?query=select+%3Fs+%3Fp+%3Fo+%7B+%3Fs+%3Fp+%3Fo+%7D"
