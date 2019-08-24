(in-package #:kgcreator)

(ql:quickload '(cl-who hunchentoot parenscript))


(setf (html-mode) :html5)

(defun write-files-run-code (a-uri raw-text)
  (if (< (length raw-text) 10)
      (list "not enough text" "not enough text")
    ;; generate random file number
    (let* ((filenum (+ 1000 (random 5000)))
	   (meta-name (concatenate 'string "temp/" (write-to-string filenum) ".meta"))
	   (text-name (concatenate 'string "temp/" (write-to-string filenum) ".txt"))
	   (rdf-name (concatenate 'string "temp/" (write-to-string filenum) ".rdf"))
	   (cypher-name (concatenate 'string "temp/" (write-to-string filenum) ".cypher"))
	   ret)
      ;; write meta file
      (with-open-file (str meta-name
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
		      (format str a-uri))
      ;; write text file
      (with-open-file (str text-name
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	(format str raw-text))
      ;; generate rdf and cypher files
      (rdf-from-files rdf-name (list (list text-name meta-name)))
      (cypher-from-files cypher-name (list (list text-name meta-name)))
      ;; read files and return results
      (setf ret
	    (list
	     (replace-all
	      (replace-all
	       (uiop:read-file-string rdf-name)
	       ">" "&gt;")
	      "<" "&lt;")
	     (uiop:read-file-string cypher-name)))
      (print (list "ret:" ret))
      ret)))
  
(defvar *h* (make-instance 'easy-acceptor :port 3000))

;; define a handler with the arbitrary name my-greetings:

(define-easy-handler (my-greetings :uri "/") (text)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((rdf-and-cypher (write-files-run-code "http://test.com/1" text)))
    (print (list "*** rdf-and-cypher:" rdf-and-cypher))
    (with-html-output-to-string
     (*standard-output* nil :prologue t)
     (:html
      (:head (:title "KGCreator Demo")
	     (:link :rel "stylesheet" :href "styles.css" :type "text/css"))
      (:body
       :style "margin: 90px"
       (:h1 "Enter plain text for the demo to create RDF and Cypher")
       (:p "For more information on the KGCreator product please visit the web site:"
	   (:a :href "https://markwatson.com/products/" "Mark Watson's commercial products"))
       (:p "The KGCreator product is a command line tool that processes all text files in a source "
	   "directory and produces both RDF data triples for semantic web applications and "
	   "Cypher input data files for the Neo4J graph database. "
	   "For the purposes of this demo the URI for your input text is hardwired to &lt;http://test.com/1&gt;"
	   "but the KGCreator product offers flexibility for assigning URIs to data sources and further, "
	   "creates links for relationships between input sources.")
       (:p :style "text-align: left"
	   "To try the demo paste plain text into the following form that contains information on companies, news, politics, famous people, broadcasting networks, political parties, countries and other locations, etc. ")
       (:p "Do not include and special characters or character sets:")
       (:form
	:method :post
	(:textarea
	 :rows "20"
	 :cols "90"
	 :name "text"
	 :value text)
	(:br)
	(:input :type :submit :value "Submit text to process"))
       (:h3 "RDF:")
       (:pre (str (car rdf-and-cypher)))
       (:h3 "Cypher:")
       (:pre (str (cadr rdf-and-cypher))))))))

(defun kgcweb ()
  (hunchentoot:start *h*))

