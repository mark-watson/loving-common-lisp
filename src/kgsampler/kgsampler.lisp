;; kgsampler main program

(in-package #:kgsampler)

(defun dbpedia-as-nt (query)
  (print query)
  (uiop:run-program 
   (list
    "curl" 
    (concatenate 'string
                 "https://dbpedia.org/sparql?format=text/ntriples&query=" ;; formats that work: csv, text/ntriples, text/ttl
                 (drakma:url-encode query :utf-8)))
   :output :string))

(defun construct-from-dbpedia (entity-uri-list &key (output-stream t))
  (dolist (entity-uri entity-uri-list)
    (format output-stream "~%~%# ENTITY NAME: ~A~%~%" entity-uri)
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A ?p ?o } where { ~A ?p ?o  . FILTER (lang(?o) = 'en') }" 
       entity-uri entity-uri)))
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A <http://purl.org/dc/terms/subject> ?o } where { ~A <http://purl.org/dc/terms/subject> ?o  }" 
       entity-uri entity-uri)))
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A <http://dbpedia.org/ontology/occupation> ?o } where { ~A <http://dbpedia.org/ontology/occupation> ?o  }" 
       entity-uri entity-uri)))
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A <http://dbpedia.org/ontology/occupation> ?o } where { ~A <http://dbpedia.org/ontology/occupation> ?o  }" 
       entity-uri entity-uri)))
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A <http://dbpedia.org/ontology/industry> ?o } where { ~A <http://dbpedia.org/ontology/industry> ?o  }" 
       entity-uri entity-uri)))
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A <http://dbpedia.org/ontology/keyPerson> ?o } where { ~A <http://dbpedia.org/ontology/keyPerson> ?o  }" 
       entity-uri entity-uri)))
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A <http://dbpedia.org/ontology/product> ?o } where { ~A <http://dbpedia.org/ontology/product> ?o  }" 
       entity-uri entity-uri)))
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A <http://dbpedia.org/ontology/service> ?o } where { ~A <http://dbpedia.org/ontology/service> ?o  }" 
       entity-uri entity-uri)))
    (format
     output-stream
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A <http://dbpedia.org/property/wordnet_type> ?o } where { ~A <http://dbpedia.org/property/wordnet_type> ?o  }" 
       entity-uri entity-uri)))))

(defun ensure-angle-brackets (s)
  "make sure URIs have angle brackets"
  (if (equal #\< (char s 0))
      s
      (concatenate 'string "<" s ">")))

(defun find-relations (entity-uri-list &key (output-stream t))
  (format output-stream "~%~%# DISCOVERED RELATIONSHIPS~%~%")
  (dolist (entity-uri1 entity-uri-list)
    (dolist (entity-uri2 entity-uri-list)
      (if (not (equal entity-uri1 entity-uri2))
          (let ((possible-relations
                 (mapcar #'cadar
                         (sparql::dbpedia
                          (format nil 
                                  "select ?p where { ~A ?p ~A . filter(!regex(str(?p), \"page\", \"i\"))} limit 50"
                                  entity-uri1 entity-uri2)))))
            (print "** possible-relations:") (print possible-relations)
            (dolist (pr possible-relations)
              (print pr)
              (format output-stream "~%~A ~A ~a .~%"
		      entity-uri1
		      (ensure-angle-brackets pr)
		      entity-uri2)))))))

(defun sample (entity-uri-list output-filepath)
  (with-open-file (ostream  (pathname output-filepath) :direction :output :if-exists :supersede)
    (construct-from-dbpedia entity-uri-list :output-stream ostream)
    (find-relations entity-uri-list :output-stream ostream)))

(defun create-sample-KG ()
  (kgsampler:sample
   '("<http://dbpedia.org/resource/Bill_Gates>" "<http://dbpedia.org/resource/Steve_Jobs>"
     "<http://dbpedia.org/resource/Microsoft>" "<http://dbpedia.org/resource/Melinda_Gates>"
     "<http://dbpedia.org/resource/Apple_Inc.>"
     "<http://dbpedia.org/resource/California>" "<http://dbpedia.org/resource/Seatle>"
     "<http://dbpedia.org/resource/Software>" "<http://dbpedia.org/resource/Computer>"
     "<http://dbpedia.org/resource/Artificial_Intelligence>" "<http://dbpedia.org/resource/Economy>"
     "<http://dbpedia.org/resource/Politics>" "<http://dbpedia.org/resource/Corporation>")
   "sample-KG.nt"))

#|

(kgsampler::create-sample-KG) ;; long running!

(kgsampler:sample '("<http://dbpedia.org/resource/Bill_Gates>" "<http://dbpedia.org/resource/Steve_Jobs>" "<http://dbpedia.org/resource/Microsoft>")  "test.nt")

(kgsampler::find-relations '("<http://dbpedia.org/resource/Bill_Gates>" "<http://dbpedia.org/resource/Steve_Jobs>" "<http://dbpedia.org/resource/Microsoft>"))

(kgsampler::dbpedia-as-nt "CONSTRUCT {
    <http://dbpedia.org/resource/Sedona> ?p ?o
} WHERE {
    <http://dbpedia.org/resource/Sedona> ?p ?o
}")

(kgsampler::construct-from-dbpedia '("<http://dbpedia.org/resource/Bill_Gates>"))

|#


