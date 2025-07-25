(in-package #:kgn-common)

;; stop words/noise words removal

(defvar *stop-words* (make-hash-table :test #'equal :size 4000))
(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defvar a1 (write-to-string *base-pathname*))
(defvar *current-directory* (subseq a1 3 (- (length a1) 11)))

(let (line)
    (with-open-file
        (in (concatenate 'string *current-directory* "data/stopwords.txt"))
      (dotimes (i 50000)
        (setq line (read-line in nil nil))
        (if (null line) (return))
        (setf (gethash line *stop-words*) t))))

(defun remove-stop-words (word-list)
  (remove-if
   #'(lambda (x)
     (or
       (gethash (string-downcase x) *stop-words*)
       (let ((q nil))
         (ignore-errors
           (if (numberp (read-from-string x))
               (setf q t)))
         q)))
   word-list))

(defun prompt-string (message)
  (format t "> ~s: " message)
  (read-line))

(defun string-shorten (str len &key (first-remove-stop-words nil)) ;; inspired by function "shorten" in package https://github.com/vindarel/cl-str
  (if (equal (type-of str) 'cons) (setf str (cadr str))) ; kluge: sometimes an association list is passed - fix.
  (if first-remove-stop-words
      (let* ((words (myutils:tokenize-string str))
             (words2 (kgn-common:remove-stop-words words))
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

(defun get-entity-data-helper (original-query &key (message-stream t))
  (let* ((uri-data (handle-URIs-in-query original-query))
         (query (remove-uris-from-query original-query))
         (el (entities:text->entities query))
         (people (entities:entities-people el)))
    (let* ((companies (entities:entities-companies el))
           (countries (entities:entities-countries el))
           (cities (entities:entities-cities el)))
      (let* ((products (entities:entities-products el))
             places
             companies-uri people-uri countries-uri cities-uri places-uri
             (text-object (kbnlp:make-text-object query))
             (to-place-names (kbnlp::text-place-names text-object))
             (to-people (kbnlp::text-human-names text-object)))
    
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
          entity-list)))))

;; (pprint (kgn-common:get-entity-data-helper "Bill Gates works at Microsoft"))
;; (pprint (entities:text->entities "Bill gates works at Microsoft"))

(defun entity-results->relationship-links (results &key (message-stream t))
  (let (all-uris
        relationship-statements
        (sep " -> "))
    (dolist (r results)
      (dolist (entity-data (cdr r))
        (dolist (ed entity-data)
          (setf all-uris (cons (first ed) all-uris)))))
    (dolist (e1 all-uris)
      (dolist (e2 all-uris)
        (setf e1 (check-uri e1))
        (setf e2 (check-uri e2))
        (if (not (equal e1 e2))
            (let ((l1 (dbpedia-get-relationships e1 e2))
                  (l2 (dbpedia-get-relationships e2 e1)))
              ;;(format t "~%~% + + entity-results->relationship-links:~%  e1=~A~%  e2=~A~%  l1=~A~%  l2=~A~%~%" e1 e2 l1 l2)
              (dolist (x l1)
                (setf relationship-statements (cons (list e1 e2 x) relationship-statements)))
              (dolist (x l2)
                (setf relationship-statements (cons (list e2 e1 x) relationship-statements)))))))
    (setf relationship-statements (remove-duplicates relationship-statements :test #'equal))
    ;;(terpri message-stream)
    (if (> (length relationship-statements) 0)
        (progn
          (format message-stream "~%DISCOVERED RELATIONSHIP LINKS:~%~%")
          (dolist (rs relationship-statements)
            (format message-stream "  ~43A~%" (first rs) )
            (format message-stream "    ~43A~%" (third rs))
            (format message-stream "    ~A .~%~%" (second rs))
            (terpri) message-stream)))
    relationship-statements))

;; Unused utility for future use:

(defun compare-name-to-uri (name uri)
  ;; Assume URI is in the form <http://dbpedia.org/resource/United_States_Senate_career_of_Barack_Obama>
  (let* ((last-slash-index (or (search "/" uri :from-end t) 0))
         (uri-suffix (remove #\Space (subseq uri (+ last-slash-index 1))))
         (cleaned-uri-suffix (remove #\/ uri))
         (squashed-name (string-downcase (remove #\Space name)))
         (score (float (/ (length (intersection (coerce cleaned-uri-suffix 'list) (coerce squashed-name 'list))) (length uri-suffix)))))
    (print (list uri-suffix squashed-name score))
    score))
    