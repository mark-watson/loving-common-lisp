(in-package #:sparql)


(defun wikidata (query)
  (let ((response
	 (uiop:run-program 
	  (list
	   "curl" 
	   (concatenate 'string
			"https://query.wikidata.org/bigdata/namespace/wdq/sparql?query="
			(drakma:url-encode query :utf-8)
			"&format=json"))
	  :output :string)))
    (with-input-from-string
	(s response)
      (let ((json-as-list (json:decode-json s)))
        ;;(pprint json-as-list)  ; uncomment this to see how following expression works:
        (mapcar #'(lambda (x)
                    (mapcar #'(lambda (y)
                                (list (car y) (cdaddr y))) x))  (cdr (cadadr json-as-list)))))))


(defun dbpedia (query)
  (print (list "dbpeia SPARQL:" query "\n"))
  (let ((response
	 (uiop:run-program 
	  (list
	   "curl" 
	   (concatenate 'string
                        "https://dbpedia.org/sparql?query="
			(drakma:url-encode query :utf-8)
			"&format=json"))
	  :output :string)))
    (with-input-from-string
        (s response)
      (let ((json-as-list (json:decode-json s)))
        (mapcar #'(lambda (x)
                    (mapcar #'(lambda (y)
                                (list (car y) (cdr (assoc :value (cdr y))))) x))
                (cdr (cadddr (cadr json-as-list))))))))

(defun fuseki (query &key (host "http://127.0.0.1") (port 3030) (suffix "/news/sparql"))
    (let* ((uri (format nil "~a:~a~a?query=" host port suffix))
           (response
            (uiop:run-program 
             (list
              "curl" 
              (concatenate 'string
                        uri
			(drakma:url-encode query :utf-8)
			"&format=json"))
	     :output :string)))
      ;;(print response)
      (with-input-from-string
          (s response)
	(let ((json-as-list (json:decode-json s)))
          (mapcar #'(lambda (x)
                      (mapcar #'(lambda (y)
                                  (list (car y) (cdr (assoc :value (cdr y))))) x))
                  (cdr (cadadr json-as-list)))))))

(defun graphdb (graph-name query &key (host "http://127.0.0.1") (port 7200) 
                      (suffix (concatenate 'string "/repositories/" graph-name)))
  "pass the name of a GraphDB graph and the sparql query. returns: list of list of strings,
   the first list being the binding variable names"
    (let* ((uri (format nil "~a:~a~a?query=" host port suffix))
           (response
            (uiop:run-program 
             (list
              "curl" 
              (concatenate 'string
                        uri
			(drakma:url-encode query :utf-8)
			"&format=json"))
	     :output :string)))
      (print response)
      (cl-csv:read-csv response)))


(defun agraph (query &key (host "mark:mark@127.0.0.1") (port 10035) (suffix "/repositories/news"))
    (let* ((uri (format nil "~a:~a~a?accept=application/json&user=mark&passwd=mark&query=" host port suffix))
           (response
            (uiop:run-program 
             (list
              "curl" 
              (concatenate 'string
                        uri
			(drakma:url-encode query :utf-8)
			"&format=json"))
	     :output :string)))
      (print (list
              "curl" 
              (concatenate 'string
                        uri
			(drakma:url-encode query :utf-8)
			"&format=json")))
      (print response)
      (with-input-from-string
          (s response)
	(let* ((json-as-list (json:decode-json s))
	       (var-names (mapcar #'intern (cdar json-as-list)))
	       (values (cdadr json-as-list)))
	  (mapcar #'(lambda (x) (mapcar #'list var-names x)) values)))))

(defun stardog (query &key (host "http://127.0.0.1") (port 5820) (suffix "/testdb/query"))
    (let* ((response
            (uiop:run-program 
             (concatenate 'string
              "curl  -u admin:admin -H \"Accept: application/sparql-results+json\" " 
              host ":" (write-to-string port) suffix
              " --data-urlencode query='"
              query
              "'")
	            :output :string)))
      (with-input-from-string
          (s response)
	(let* ((json-as-list (json:decode-json s))
               (var-names (cdadar json-as-list))
               (values 
                (mapcar #'(lambda (x)
                            (mapcar #'(lambda (y)
                                        (list (car y) (cdr (assoc :value (cdr y))))) x))
                        (cdr (cadadr json-as-list)))))
          (cons
           var-names
           (mapcar #'(lambda (l1)
                       (mapcar #'(lambda (l2) (second l2))
                               l1))
                   (mapcar  #'(lambda (x) (mapcar #'(lambda (vn)
                                                      (assoc (make-symbol (string-upcase vn)) x ;; simplify this?
                                                             :test #'(lambda (a b)
                                                                       (equal (subseq (write-to-string a) 1)
                                                                              (write-to-string b)))))
                                                  var-names)) values)))))))

#|
(setf dd (sparql:wikidata "select ?s ?p { ?s ?p \"Bill Gates\"@en }"))
(pprint dd)
(setf rr (sparql:dbpedia "select ?s ?p { ?s ?p \"Bill Gates\"@en }"))
(pprint rr)

(sparql::fuseki "select ?s ?p ?o { ?s ?p ?o }")

(sparql::stardog "select ?s ?p ?o { ?s ?p ?o } limit 20")

 (sparql::graphdb "KBS" "select ?s ?p ?o { ?s ?p ?o } limit 20")

 (sparql::agraph "select ?s ?p ?o { ?s ?p ?o }")
|#

;; curl "http://127.0.0.1:3030/news/?query=select+%3Fs+%3Fp+%3Fo+%7B+%3Fs+%3Fp+%3Fo+%7D"
