;; kgsampler main program

(in-package #:kgsampler)

(defun dbpedia-as-nt (query)
  "Query DBpedia and return results in N-Triples format."
  (print query)
  (uiop:run-program 
   (list
    "curl" 
    (concatenate 'string
                 "https://dbpedia.org/sparql?format=text/ntriples&query=" ;; formats that work: csv, text/ntriples, text/ttl
                 (drakma:url-encode query :utf-8)))
   :output :string))

(defparameter *construct-predicates*
  '("<http://purl.org/dc/terms/subject>"
    "<http://dbpedia.org/ontology/occupation>"
    "<http://dbpedia.org/ontology/industry>"
    "<http://dbpedia.org/ontology/keyPerson>"
    "<http://dbpedia.org/ontology/product>"
    "<http://dbpedia.org/ontology/service>"
    "<http://dbpedia.org/property/wordnet_type>")
  "Predicates (in addition to English language literals) fetched for each entity.")

(defun construct-from-dbpedia (entity-uri-list &key (output-stream t))
  "For a list of DBpedia entity URIs, fetch all triples for each and write to a stream."
  ;; note: use write-string, not format, since the N-Triples data may contain ~ characters
  (dolist (entity-uri entity-uri-list)
    (format output-stream "~%~%# ENTITY NAME: ~A~%~%" entity-uri)
    (write-string
     (dbpedia-as-nt
      (format
       nil
       "CONSTRUCT { ~A ?p ?o } where { ~A ?p ?o  . FILTER (lang(?o) = 'en') }"
       entity-uri entity-uri))
     output-stream)
    (dolist (predicate *construct-predicates*)
      (write-string
       (dbpedia-as-nt
        (format
         nil
         "CONSTRUCT { ~A ~A ?o } where { ~A ~A ?o  }"
         entity-uri predicate entity-uri predicate))
       output-stream))))

(defun ensure-angle-brackets (s)
  "make sure URIs have angle brackets"
  (if (equal #\< (char s 0))
      s
      (concatenate 'string "<" s ">")))

(defun find-relations (entity-uri-list &key (output-stream t))
  "Discover relationships between pairs of entities in a list of URIs and write them to a stream."
  (format output-stream "~%~%# DISCOVERED RELATIONSHIPS~%~%")
  (dolist (entity-uri1 entity-uri-list)
    (dolist (entity-uri2 entity-uri-list)
      (if (not (equal entity-uri1 entity-uri2))
          (let ((possible-relations
                 (mapcar #'cadar
                         (sparql:dbpedia
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
  "Generate a sample knowledge graph from a list of DBpedia entities and save to a file."
  (with-open-file (ostream  (pathname output-filepath) :direction :output :if-exists :supersede)
    (construct-from-dbpedia entity-uri-list :output-stream ostream)
    (find-relations entity-uri-list :output-stream ostream)))

(defun create-sample-KG ()
  "Create a sample knowledge graph with a predefined set of entities."
  (kgsampler:sample
   '("<http://dbpedia.org/resource/Bill_Gates>" "<http://dbpedia.org/resource/Steve_Jobs>"
     "<http://dbpedia.org/resource/Microsoft>" "<http://dbpedia.org/resource/Melinda_Gates>"
     "<http://dbpedia.org/resource/Apple_Inc.>"
     "<http://dbpedia.org/resource/California>" "<http://dbpedia.org/resource/Seattle>"
     "<http://dbpedia.org/resource/Software>" "<http://dbpedia.org/resource/Computer>"
     "<http://dbpedia.org/resource/Artificial_intelligence>" "<http://dbpedia.org/resource/Economy>"
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


