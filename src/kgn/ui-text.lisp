(in-package #:kgn)

(defun prompt-string (message)
  (format t "> ~s: " message)
  (read-line))

(defun string-shorten (str len &key (first-remove-stop-words nil)) ;; inspired by function "shorten" in package https://github.com/vindarel/cl-str
  (if first-remove-stop-words
      (let* ((words (tokenize-string str))
             (words2 (remove-stop-words words))
             (s2 (format nil "~{~a~^ ~}" words2)))
        (setf str s2)))
  (when  (< len (length str))
    (let ((end (max (- len 3) 0)))
      (loop while (and (> end (/ len 2)) (not (equal (subseq str end (+ end 1)) " "))) do
             (setf end (- end 1)))
      (setf str (concatenate 'string (subseq str 0 end) "..."))))
  str)

(defun display-entity-results (output-stream r-list)
  (dolist (r r-list)
    (format output-stream "~%~%entity result:~%~S~%" r)
    (dolist (val r)
      (if (> (length (second val)) 0)
          (format output-stream "~%~a: ~a~%" (first val) (second val))))))

(defun get-URIs-in-query (query) ;; URIs contain < > brackets
  (let (ret
        w
        (ll (coerce query 'list))
        in-uri)
    (dolist (ch ll)
      (if in-uri
          (if (equal ch #\>)
              (setf w (cons ch w)
                    ret (cons (coerce (reverse w) 'string) ret)
                    in-uri nil
                    w nil)
            (setf w (cons ch w))))
      (if (equal ch #\<) (setf in-uri t
                               w (cons #\< w))))
    ret))

(defun remove-uris-from-query (query) ;; URIs contain < > brackets
  (let (ret
         (ll (coerce query 'list))
        in-uri)
    (dolist (ch ll)
      (if (equal ch #\<) (setf in-uri t))
      (if (not in-uri)
           (setf ret (cons ch ret)))
       (if (equal ch #\>) (setf in-uri nil)))
    (coerce (reverse ret) 'string)))

(print (remove-uris-from-query "<http://dbpedia.org/resource/Bill_Gates> visited <http://dbpedia.org/resource/Apple_Inc.>"))

(defun handle-URIs-in-query (query)
  (let* ((uris (get-URIs-in-query query))
         (entity-names (map 'list #'get-name-and-description-for-uri uris)))
    (mapcar #'list uris (map 'list #'second entity-names))))

;;(print (handle-URIs-in-query "<http://dbpedia.org/resource/Bill_Gates> visited <http://dbpedia.org/resource/Apple_Inc.>"))

(defun get-entity-data-helper (original-query &key (message-stream t) (updater nil)) ;; updator for progress bar
  (let* ((uri-data (handle-URIs-in-query original-query))
         (query (remove-uris-from-query original-query))
         ret
         (el (entities:text->entities query))
         (people (entities:entities-people el)))
    (if updater
        (let ()
          (setf *percent* (+ *percent* 2))
          (funcall updater *percent*)))
    (let* ((companies (entities:entities-companies el))
           (countries (entities:entities-countries el))
           (cities (entities:entities-cities el)))
      (if updater
          (let ()
            (setf *percent* (+ *percent* 2))
            (funcall updater *percent*)))
      (let* ((products (entities:entities-products el))
             places
             companies-uri people-uri countries-uri cities-uri places-uri
             (text-object (kbnlp:make-text-object query))
             (to-place-names (kbnlp::text-place-names text-object))
             (to-people (kbnlp::text-human-names text-object)))

        (if updater
          (let ()
            (setf *percent* (+ *percent* 3))
            (funcall updater *percent*)))
    
        (dolist (ud uri-data)
          (if (ask-is-type-of (first ud) "<http://dbpedia.org/ontology/Company>")
              (setf companies-uri (cons ud companies-uri)))
          (if (ask-is-type-of (first ud) "<http://dbpedia.org/ontology/Person>")
              (setf people-uri (cons ud people-uri)))
          (if (ask-is-type-of (first ud) "<http://dbpedia.org/ontology/Country>")
              (setf countries-uri (cons ud countries-uri)))
          (if (ask-is-type-of (first ud) "<http://dbpedia.org/ontology/City>")
              (setf cities-uri (cons ud cities-uri)))
          (if (ask-is-type-of (first ud) "<http://dbpedia.org/ontology/Place>")
              (setf places-uri (cons ud places-uri))))
        (dolist (place to-place-names)
          (if (and
               (not (member place countries :test #'equal))
               (not (member place cities :test #'equal)))
              (setf places (cons place places))))
        (dolist (person to-people)
          (if (not (member person people :test #'equal)) (setf people (cons person people))))
        (let ((entity-list
               (list
                (cons :people
                      (append
                       (loop for person in people collect
                             (dbpedia-get-entities-by-name
                              person "<http://dbpedia.org/ontology/Person>" "<http://schema.org/Person>" :message-stream message-stream))
                       (list people-uri)))
                (cons :countries
                      (append
                       (loop for country in countries collect
                             (dbpedia-get-entities-by-name
                              country "<http://dbpedia.org/ontology/Country>" "<http://schema.org/Country>" :message-stream message-stream))
                       (list countries-uri)))
                (cons :cities
                      (append
                       (loop for city in cities collect
                             (dbpedia-get-entities-by-name
                              city "<http://dbpedia.org/ontology/City>" "<http://schema.org/City>" :message-stream message-stream))
                       (list cities-uri)))
                (cons :places
                      (append
                       (loop for place in places collect
                             (dbpedia-get-entities-by-name
                              place "<http://dbpedia.org/ontology/Place>" "<http://schema.org/Place>" :message-stream message-stream))
                       (list places-uri)))
                (cons :products
                      (loop for product in products collect
                            (dbpedia-get-entities-by-name ;; fix, no <http://dbpedia.org/ontology/Product>:
                             product "<http://dbpedia.org/ontology/Product>" "<http://schema.org/Product>" :message-stream message-stream)))
                (cons :companies
                      (append
                       (loop for company in companies collect
                             (dbpedia-get-entities-by-name ;; fix: no "<http://dbpedia.org/ontology/Organization>"
                              company "<http://dbpedia.org/ontology/Organization>" "<http://schema.org/Organization>"
                              :message-stream message-stream))
                       (list companies-uri))))))
          (format t "~%~%    entity-list:~%~%") (pprint entity-list) (format t "~%~%~%")
          (setf ret (prompt-selection-list entity-list))
          (format t "~%~%--------- ret:~%~%~S~%~%" ret)
          ret)))))

(defun entity-results->relationship-links (results &key (message-stream t) (updater nil))
(print "+++++++++++++++++++")
(pprint results)
  (let (all-uris
        relationship-statements
        (sep " -> "))
    (dolist (r results)
      (dolist (entity-data (cdr r))
        (dolist (ed entity-data)
          (setf all-uris (cons (first ed) all-uris)))))
    (dolist (e1 all-uris)
      (dolist (e2 all-uris)
        (if updater
            (let ()
              (setf *percent* (+ *percent* 1))
              (funcall updater *percent*)))
        (if (not (equal e1 e2))
            (let ((l1 (dbpedia-get-relationships e1 e2))
                  (l2 (dbpedia-get-relationships e2 e1)))
              (dolist (x l1)
                (print (list "x l1:" x))
                (setf relationship-statements (cons (list e1 e2 x) relationship-statements)))
              (dolist (x l2)
                (print (list "x l2:" x))
                (setf relationship-statements (cons (list e2 e1 x) relationship-statements)))))))
    (setf relationship-statements (remove-duplicates relationship-statements :test #'equal))
    ;;(terpri message-stream)
    (capi::write-string-with-properties "DISCOVERED RELATIONSHIP LINKS:"  '(:highlight :compiler-warning-highlight) message-stream)
    (terpri message-stream) (terpri message-stream)
    (dolist (rs relationship-statements)
      (format message-stream "~43A" (first rs))
      (capi::write-string-with-properties sep '(:highlight :compiler-warning-highlight) message-stream)
      (format message-stream "~43A" (third rs))
      (capi::write-string-with-properties sep '(:highlight :compiler-warning-highlight) message-stream)
      (format message-stream "~A" (second rs))
      (terpri message-stream))
    relationship-statements))


(defun t1 ()
  (pprint (get-entity-data-helper "Bill Gates and Ian Smith work for IBM and went to London and England to buy a Pepsi")))

