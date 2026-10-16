;; Copyright 2021-2022 Mark Watson All Rights Reserved.
;; License: Apache 2

(in-package #:sparql-cache)

;; caching code:

;; one time only: create empty cache database: sqlite3 ~/.kgn_cache.db

;;; SQLite caching for SPARQL queries:

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

(defun fetch-sparql-json (endpoint query)
  "Run QUERY against the SPARQL ENDPOINT URL and return the JSON response string."
  (replace-all
   (replace-all
    (uiop:run-program
     (list
      "curl"
      (concatenate 'string
                   endpoint
                   "?query="
                   (drakma:url-encode query :utf-8)
                   "&format=json"))
     :output :string)
    "\\u2013" " ")
   "\\u" " "))

(defun json-bindings->lists (response)
  "Decode a SPARQL JSON result string into a list of rows, each row a list of
(variable-keyword value) pairs."
  (with-input-from-string (s response)
    (let* ((json-as-list (json:decode-json s))
           (bindings (cdr (assoc :bindings
                                 (cdr (assoc :results json-as-list))))))
      ;;(pprint json-as-list)  ; uncomment this to see how following expression works:
      (mapcar #'(lambda (x)
                  (mapcar #'(lambda (y)
                              (list (car y) (cdr (assoc :value (cdr y))))) x))
              bindings))))

(defun wikidata (query)
  (let* ((cr (fetch-result-wikidata query))
         (response (or cr (fetch-sparql-json "https://query.wikidata.org/bigdata/namespace/wdq/sparql" query))))
    (unless cr (save-query-result-wikidata query response))
    (json-bindings->lists response)))

(defun dbpedia (query)
  (let* ((cr (fetch-result-dbpedia query))
         (response (or cr (fetch-sparql-json "https://dbpedia.org/sparql" query))))
    (unless cr (save-query-result-dbpedia query response))
    (ignore-errors (json-bindings->lists response))))

(defun ask-dbpedia (query)
  (let* ((cr (fetch-result-dbpedia query))
         (response (or cr (fetch-sparql-json "https://dbpedia.org/sparql" query))))
    (unless cr (save-query-result-dbpedia query response))
    (if (search "true" response) t nil)))

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
