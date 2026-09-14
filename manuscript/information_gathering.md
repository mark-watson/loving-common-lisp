# Information Gathering Using DBPedia Lookup {#information_gathering}

Wikipedia is a great source of information. As you may know, you can download a [data dump of all Wikipedia data](https://en.wikipedia.org/wiki/Wikipedia:Database_download) with or without version information and comments. When I want fast access to the entire Wikipedia set of English language articles I choose the second option and just get the current pages with no comments of versioning information. [This is the direct download link for current Wikipedia articles.](http://download.wikimedia.org/enwiki/latest/enwiki-latest-pages-articles.xml.bz2) There are no comments or user pages in this GZIP file. This is not as much data as you might think, only about 9 gigabytes compressed or about 42 gigabytes uncompressed.

To load and run an example, try:

{lang="lisp",linenos=off}
~~~~~~~~
(ql:quickload "dbpedia")
(dbpedia:dbpedia-lookup "berlin")
~~~~~~~~

Wikipedia is a great resource to have on hand but I am going to show you in this section how to access the Semantic Web version or Wikipedia, [DBPedia](http://dbpedia.org/) using the DBPedia Lookup Service in the next code listing that shows the contents of the example file **dbpedia-lookup.lisp** in the directory **src/dbpedia**:

{lang="lisp",linenos=on}
~~~~~~~~
(in-package #:dbpedia)

;; utility from http://cl-cookbook.sourceforge.net/strings.html#manip:
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurrences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defstruct dbpedia-data uri label description)

(defun dbpedia-lookup (search-string)
  (let* ((s-str (replace-all search-string " " "+"))
         (s-uri
          (concatenate
           'string
           "https://lookup.dbpedia.org/api/search?query="
           s-str))
         (response-body nil)
         (response-status nil)
         (response-headers nil)
         (xml nil)
         ret)
    (multiple-value-setq
        (response-body response-status response-headers)
      (drakma:http-request
       s-uri
       :method :get
       :accept "application/xml"))
    (unless (= response-status 200)
      (error "DBpedia lookup failed with status ~A" response-status))
    (let ((xml-str (typecase response-body
                     (string response-body)
                     (vector (babel:octets-to-string response-body))
                     (t (error "Invalid or empty response body from DBpedia")))))
      (setf xml (s-xml:parse-xml-string xml-str)))
    (dolist (r (cdr xml))
      (let ((uri (cadr (find "uri" (cdr r)
                             :key (lambda (el) (and (consp el) (symbol-name (first el))))
                             :test #'string-equal)))
            (label (cadr (find "Label" (cdr r)
                               :key (lambda (el) (and (consp el) (symbol-name (first el))))
                               :test #'string-equal)))
            (desc (cadr (find "Description" (cdr r)
                              :key (lambda (el) (and (consp el) (symbol-name (first el))))
                              :test #'string-equal))))
        (push
         (make-dbpedia-data
          :uri uri
          :label label
          :description (and desc (string-trim '(#\Space #\NewLine #\Tab) desc)))
         ret)))
    (reverse ret)))

;; (dbpedia:dbpedia-lookup "berlin")
~~~~~~~~

I am only capturing the attributes for DBPedia URI, label and description in this example code. If you uncomment line 41 and look at the entire response body from the call to DBPedia Lookup, you can see other attributes that you might want to capture in your applications.


The following diagram shows the high-level architecture of the information gathering tools developed in this chapter:

{width: "80%"}
![Architecture diagram](images/information_gathering_architecture.png)

Here is a sample call to the function **dbpedia:dbpedia-lookup** (only some of the returned data is shown):

~~~~~~~~
* (ql:quickload "dbpedia")
* (dbpedia:dbpedia-lookup "berlin")

(#S(DBPEDIA-DATA
  :URI "http://dbpedia.org/resource/Berlin"
  :LABEL "Berlin"
  :DESCRIPTION
  "Berlin is the capital city of Germany and one of the 16 states of Germany.
   With a population of 3.5 million people, Berlin is Germany's largest city
   and is the second most populous city proper and the eighth most populous
   urban area in the European Union. Located in northeastern Germany, it is
   the center of the Berlin-Brandenburg Metropolitan Region, which has 5.9
   million residents from over 190 nations. Located in the European Plains,
   Berlin is influenced by a temperate seasonal climate.")
 ...)
~~~~~~~~

Wikipedia, and the DBPedia linked data for of Wikipedia are great sources of online data. If you get creative, you will be able to think of ways to modify the systems you build to pull data from DPPedia. One warning: Semantic Web/Linked Data sources on the web are not available 100% of the time. If your business applications depend on having the DBPedia always available then you can follow the instructions on the [DBPedia web site](http://dbpedia.org) to install the service on one of your own servers.

## Wrap Up

DBPedia Lookup is a convenient way to get information on entities in Wikipedia and DBPedia. In the chapter **Web Scraping** we look at ways to pull data directly off web sites.


