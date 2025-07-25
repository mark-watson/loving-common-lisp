# Knowledge Graph Navigator {#kgn}


The Knowledge Graph Navigator (which I will often refer to as KGN) is a tool for processing a set of entity names and automatically exploring the public Knowledge Graph [DBPedia](http://dbpedia.org) using SPARQL queries. I started to write KGN for my own use, to automate some things I used to do manually when exploring Knowledge Graphs, and later thought that KGN might be useful also for educational purposes. KGN shows the user the auto-generated SPARQL queries so hopefully the user will learn by seeing examples. KGN uses NLP code developed in earlier chapters and we will reuse that code with a short review of using the APIs.

In previous versions of this book, this example was hard-wired to use LispWork CAPI for the user interface. This old version is in **src/kgn** and has a few UI components like a progress bar that I removed. The new version has a library component in **src/kgn-common**, a text UI component in **src/kgn-text-ui**, a LispWorks CAPI component in **src/kgn-capi-ui**, and a McCLIM version in **src/kgn-mcclim-ui**.

{width=90%}
![UI for the Knowledge Graph Navigator](images/kgn1.png)

After looking at generated SPARQL for an example query use of the application, we will start a process of bottom up development, first writing low level functions to automate SPARQL queries, writing utilities we will need for the UI, and finally writing the UI. Some of the problems we will need to solve along the way will be colorizing the output the user sees in the UI and implementing a progress bar so the application user does not think the application is "hanging" while generating and making SPARQL queries to DBPedia.

Since the DBPedia queries are time consuming, we will also implement a caching layer using SQLite that will make the app more responsive. The cache is especially helpful during development when the same queries are repeatedly used for testing.

The code for this application is in the directory **src/kgn**. KGN is a long example application for a book and we will not go over all of the code. Rather, I hope to provide you with a roadmap overview of the code, diving in on code that you might want to reuse for your own projects and some representative code for generating SPARQL queries.

## Example Output

Before we get started studying the implementation, let's look at sample output in order to help give meaning to the code we will look at later. Consider a query that a user might type into the top query field in the KGN app:

        Steve Jobs lived near San Francisco and was
        a founder of \<http://dbpedia.org/resource/Apple_Inc.\>

The system will try to recognize entities in a query. If you know the DBPedia URI of an entity, like the company Apple in this example, you can use that directly. Note that in the SPARQL  URIs are surrounded with angle bracket characters.

The application prints out automatically generated SPARQL queries. For the above listed example query the following output will be generated (some editing to fit page width):

{linenos=off}
~~~~~~~~
Trying to get entity by name = Steve Jobs using SPARQL with type:
~~~~~~~~

{lang="sparql",linenos=off}
~~~~~~~~
select distinct ?s ?comment { ?s ?p "Steve Jobs"@en . 
 ?s <http://www.w3.org/2000/01/rdf-schema#comment> ?comment .
   FILTER ( lang ( ?comment ) = 'en' ) . 
 ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
    <http://dbpedia.org/ontology/Person> . 
 } LIMIT 15 
~~~~~~~~

{linenos=off}
~~~~~~~~
Trying to get entity by name = San Francisco using SPARQL with type:
~~~~~~~~
{lang="sparql",linenos=off}
~~~~~~~~
select distinct ?s ?comment { ?s ?p "San Francisco"@en . 
 ?s <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . 
   FILTER ( lang ( ?comment ) = 'en' ) . 
 ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
    <http://dbpedia.org/ontology/City> . 
 } LIMIT 15 
~~~~~~~~
{linenos=off}
~~~~~~~~
SPARQL to get PERSON data for <http://dbpedia.org/resource/Steve_Jobs>:
~~~~~~~~
{lang="sparql",linenos=off}
~~~~~~~~
SELECT DISTINCT ?label ?comment 
 ( GROUP_CONCAT ( DISTINCT ?birthplace ; SEPARATOR=' | ' ) AS ?birthplace ) 
 ( GROUP_CONCAT ( DISTINCT ?almamater ; SEPARATOR=' | ' ) AS ?almamater ) 
 ( GROUP_CONCAT ( DISTINCT ?spouse ; SEPARATOR=' | ' ) AS ?spouse ) { 
 <http://dbpedia.org/resource/Steve_Jobs> 
   <http://www.w3.org/2000/01/rdf-schema#comment>
   ?comment . 
 FILTER ( lang ( ?comment ) = 'en' ) . 
 OPTIONAL { <http://dbpedia.org/resource/Steve_Jobs>
            <http://dbpedia.org/ontology/birthPlace>
            ?birthplace } . 
 OPTIONAL { <http://dbpedia.org/resource/Steve_Jobs> 
            <http://dbpedia.org/ontology/almaMater> 
            ?almamater } . 
 OPTIONAL { <http://dbpedia.org/resource/Steve_Jobs> 
            <http://dbpedia.org/ontology/spouse> 
            ?spouse } . 
 OPTIONAL { <http://dbpedia.org/resource/Steve_Jobs> 
            <http://www.w3.org/2000/01/rdf-schema#label> 
            ?label . 
 FILTER ( lang ( ?label ) = 'en' ) } 
 } LIMIT 10 
~~~~~~~~
{linenos=off}
~~~~~~~~
SPARQL to get CITY data for <http://dbpedia.org/resource/San_Francisco>:
~~~~~~~~
{lang="sparql",linenos=off}
~~~~~~~~
SELECT DISTINCT ?label ?comment 
 ( GROUP_CONCAT ( DISTINCT ?latitude_longitude ; SEPARATOR=' | ' ) 
     AS ?latitude_longitude ) 
 ( GROUP_CONCAT ( DISTINCT ?populationDensity ; SEPARATOR=' | ' ) 
     AS ?populationDensity ) 
 ( GROUP_CONCAT ( DISTINCT ?country ; SEPARATOR=' | ' ) 
     AS ?country ) { 
 <http://dbpedia.org/resource/San_Francisco> 
   <http://www.w3.org/2000/01/rdf-schema#comment>
   ?comment . 
      FILTER ( lang ( ?comment ) = 'en' ) . 
 OPTIONAL { <http://dbpedia.org/resource/San_Francisco> 
            <http://www.w3.org/2003/01/geo/wgs84_pos#geometry> 
            ?latitude_longitude } . 
 OPTIONAL { <http://dbpedia.org/resource/San_Francisco> 
            <http://dbpedia.org/ontology/PopulatedPlace/populationDensity> 
            ?populationDensity } . 
 OPTIONAL { <http://dbpedia.org/resource/San_Francisco> 
            <http://dbpedia.org/ontology/country> 
            ?country } . 
 OPTIONAL { <http://dbpedia.org/resource/San_Francisco> 
            <http://www.w3.org/2000/01/rdf-schema#label> 
            ?label . } 
 } LIMIT 30 
~~~~~~~~
{linenos=off}
~~~~~~~~
SPARQL to get COMPANY data for <http://dbpedia.org/resource/Apple_Inc.>:
~~~~~~~~
{lang="sparql",linenos=off}
~~~~~~~~
SELECT DISTINCT ?label ?comment ( GROUP_CONCAT ( DISTINCT ?industry ; SEPARATOR=' | ' ) 
      AS ?industry ) 
 ( GROUP_CONCAT ( DISTINCT ?netIncome ; SEPARATOR=' | ' ) 
      AS ?netIncome ) 
 ( GROUP_CONCAT ( DISTINCT ?numberOfEmployees ; SEPARATOR=' | ' ) 
      AS ?numberOfEmployees ) { 
 <http://dbpedia.org/resource/Apple_Inc.> 
    <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . 
 FILTER ( lang ( ?comment ) = 'en' ) . 
 OPTIONAL { <http://dbpedia.org/resource/Apple_Inc.> 
            <http://dbpedia.org/ontology/industry> 
            ?industry } . 
 OPTIONAL { <http://dbpedia.org/resource/Apple_Inc.> 
            <http://dbpedia.org/ontology/netIncome> ?netIncome } . 
 OPTIONAL { <http://dbpedia.org/resource/Apple_Inc.> 
            <http://dbpedia.org/ontology/numberOfEmployees> ?numberOfEmployees } . 
 OPTIONAL { <http://dbpedia.org/resource/Apple_Inc.> 
            <http://www.w3.org/2000/01/rdf-schema#label> ?label . 
              FILTER ( lang ( ?label ) = 'en' ) } 
 } LIMIT 30 
~~~~~~~~

{linenos=off}
~~~~~~~~
DISCOVERED RELATIONSHIP LINKS:
~~~~~~~~

{lang="sparql",linenos=off}
~~~~~~~~
<http://dbpedia.org/resource/Steve_Jobs>    ->
    <http://dbpedia.org/ontology/birthPlace>    ->
    <http://dbpedia.org/resource/San_Francisco>
<http://dbpedia.org/resource/Steve_Jobs>    -> 
    <http://dbpedia.org/ontology/occupation>    -> 
    <http://dbpedia.org/resource/Apple_Inc.>
<http://dbpedia.org/resource/Steve_Jobs>    -> 
    <http://dbpedia.org/ontology/board>         -> 
    <http://dbpedia.org/resource/Apple_Inc.>
<http://dbpedia.org/resource/Steve_Jobs>    -> 
    <http://www.w3.org/2000/01/rdf-schema#seeAlso> -> 
    <http://dbpedia.org/resource/Apple_Inc.>
<http://dbpedia.org/resource/Apple_Inc.>    -> 
    <http://dbpedia.org/property/founders>      -> 
    <http://dbpedia.org/resource/Steve_Jobs>
~~~~~~~~

After listing the generated SPARQL for finding information for the entities in the query, KGN searches for relationships between these entities. These discovered relationships can be seen at the end of the last listing. Please note that this step makes SPARQL queries on **O(n^2)** where **n** is the number of entities. Local caching of SPARQL queries to DBPedia helps make processing several entities possible.

In addition to showing generated SPARQL and discovered relationships in the middle text pane of the application, KGN also generates formatted results that are also displayed in the bottom text pane:

{linenos=off}
~~~~~~~~

- - - ENTITY TYPE: PEOPLE - - -

LABEL: Steve Jobs

COMMENT: Steven Paul "Steve" Jobs was an American information technology 
entrepreneur and inventor. He was the co-founder, chairman, and chief 
executive officer (CEO) of Apple Inc.; CEO and majority shareholder
of Pixar Animation Studios; a member of The Walt Disney Company's 
board of directors following its acquisition of Pixar; and founder, 
chairman, and CEO of NeXT Inc. Jobs is widely recognized as a pioneer of 
the microcomputer revolution of the 1970s and 1980s, along with Apple 
co-founder Steve Wozniak. Shortly after his death, Jobs's official 
biographer, Walter Isaacson, described him as a "creative entrepreneur 
whose passion for perfection and ferocious drive revolutionized six industries:
personal computers, animated movies, music, phones

BIRTHPLACE: http://dbpedia.org/resource/San_Francisco

ALMAMATER: http://dbpedia.org/resource/Reed_College

SPOUSE: http://dbpedia.org/resource/Laurene_Powell_Jobs

- - - ENTITY TYPE: CITIES - - -

LABEL:  San Francisco

COMMENT: San Francisco, officially the City and County of San Francisco, is the
cultural, commercial, and financial center of Northern California and
the only consolidated city-county in California. San Francisco encompasses a
land area of about 46.9 square miles (121 km2) on the northern end of the 
San Francisco Peninsula, which makes it the smallest county in the state. 
It has a density of about 18,451 people per square mile (7,124 people per km2),
making it the most densely settled large city (population greater than 
200,000) in the state of California and the second-most densely populated 
major city in the United States after New York City. San Francisco is 
the fourth-most populous city in California, after Los Angeles, San Diego, and 
San Jose, and the 13th-most populous cit

LATITUDE--LONGITUDE: POINT(-122.41666412354 37.783332824707)

POPULATION-DENSITY: 7123.97092726667

COUNTRY: http://dbpedia.org/resource/United_States

- - - ENTITY TYPE: COMPANIES - - -

LABEL: Apple Inc.

COMMENT: Apple Inc. is an American multinational technology company headquartered
in Cupertino, 
California, that designs, develops, and sells consumer electronics, 
computer software, and online services. Its hardware products include the 
iPhone smartphone, the iPad tablet computer, the Mac personal computer, the 
iPod portable media player, the Apple Watch smartwatch, and the Apple TV digital 
media player. Apple's consumer software includes the macOS and iOS operating 
systems, the iTunes media player, the Safari web browser, and the iLife and 
iWork creativity and productivity suites. Its online services include the 
iTunes Store, the iOS App Store and Mac App Store, Apple Music, and iCloud.

INDUSTRY: http://dbpedia.org/resource/Computer_hardware | 
          http://dbpedia.org/resource/Computer_software | 
          http://dbpedia.org/resource/Consumer_electronics | 
          http://dbpedia.org/resource/Corporate_Venture_Capital | 
          http://dbpedia.org/resource/Digital_distribution |
          http://dbpedia.org/resource/Fabless_manufacturing

NET-INCOME: 5.3394E10

NUMBER-OF-EMPLOYEES: 115000
~~~~~~~~

Hopefully after reading through sample output and seeing the screen shot of the application, you now have a better idea what this example application does. Now we will look at project configuration and then implementation.

## Project Configuration and Running the Application

The following listing of **kgn.asd** shows the ten packages this example depends on (five of these are also examples in this book, and five are in the public Quicklisp repository):

{lang="lisp",linenos=on}
~~~~~~~~
;;;; knowledgegraphnavigator.asd

(asdf:defsystem #:kgn
  :description "Describe dbpedia here"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:sqlite #:cl-json #:alexandria #:drakma #:myutils #:lw-grapher 
               #:trivial-open-browser #:entities #:entity-uris #:kbnlp)
  :components ((:file "package")
               (:file "ui-text")
               (:file "utils")
               (:file "sparql")
               (:file "colorize")
               (:file "user-interface")
               (:file "option-pane")
               (:file "kgn")
               (:file "gui")
               (:file "nlp")
               (:file "sparql-results-to-english")
               (:file "gen-output")))
~~~~~~~~

You are probably aware of many of the dependency libraries used here but you may not have seen **trivial-open-browser** which we will use to open a web browser to URIs for human readable information on DBPedia.

Listing of **package.lisp**:

{lang="lisp",linenos=on}
~~~~~~~~
;;;; package.lisp

(defpackage #:kgn
  (:use #:cl #:alexandria #:myutils #:sqlite #:myutils
   #:lw-grapher #:trivial-open-browser #:entities #:entity-uris
   #:kbnlp #:CAPI)
  (:export #:kgn))
~~~~~~~~

The free personal edition of LispWorks does not support initialization files so you must manually load Quicklisp from the Listener Window when you first start LispWorks Personal as seen in the following repl listing (edited to remove some output for brevity). Once Quicklisp is loaded we then use **ql:quickload** to load the example in this chapter (some output removed for brevity):

{lang="lisp",linenos=off}
~~~~~~~~
CL-USER 1 > (load "~/quicklisp/setup.lisp")
; Loading text file /Users/markw/quicklisp/setup.lisp
; Loading /Applications/LispWorks Personal 7.1/...
;; Creating system "COMM"
#P"/Users/markw/quicklisp/setup.lisp"

CL-USER 2 > (ql:quickload "kgn")
To load "kgn":
  Load 1 ASDF system:
    kgn
; Loading "kgn"
.
"Starting to load data...." 
"....done loading data." 
"#P\"/Users/markw/GITHUB/common-lisp/entity-uris/entity-uris.lisp\"" 
"current directory:" 
"/Users/markw/GITHUB/common-lisp/entity-uris" 
"Starting to load data...." 
"....done loading data."
[package kgn]
To load "sqlite":
  Load 1 ASDF system:
    sqlite
; Loading "sqlite"
To load "cl-json":
  Load 1 ASDF system:
    cl-json
; Loading "cl-json"
To load "drakma":
  Load 1 ASDF system:
    drakma
; Loading "drakma"
.To load "entity-uris":
  Load 1 ASDF system:
    entity-uris
; Loading "entity-uris"
("kgn")
CL-USER 3 > (kgn:kgn)
#<KGN::KGN-INTERFACE "Knowledge Graph Navigator" 40201E91DB>
~~~~~~~~

Please note that I assume that you have configured all of the examples for this book for discoverability by Quicklisp as per the section [Setup for Local Quicklisp Projects](#qlconfig) in Appendix A.

When the KGN application starts a sample query is randomly chosen. Queries with many entities can take a while to process, especially when you first start using this application. Every time KGN makes a web service call to DBPedia the query and response are cached in a SQLite database in **~/.kgn_cache.db** which can greatly speed up the program, especially in development mode when testing a set of queries. This caching also takes some load off of the public DBPedia endpoint, which is a polite thing to do.

I use LispWorks Professional and add two utility functions to the bottom on my **~/.lispworks** configuration file (you can't do this with LispWorks Personal):

{lang="lisp",linenos=on}
~~~~~~~~
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init
         (merge-pathnames
           "quicklisp/setup.lisp"
           (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun ql (x) (ql:quickload x))
(defun qlp (x)
  (ql:quickload x)
  (SYSTEM::%IN-PACKAGE (string-upcase x) :NEW T))
~~~~~~~~

Function **ql** is just a short alias to avoid frequently typing **ql:quickload** and **qlp** loads a Quicklisp project and then performs an **in-package** of the Common Lisp package with the same name as the Quicklisp project.

## Review of NLP Utilities Used in Application

Here is a quick review of NLP utilities we saw earlier:

- kbnlp:make-text-object
- kbnlp::text-human-names
- kbnlp::text-place-name
- entity-uris:find-entities-in-text
- entity-uris:pp-entities

The following code snippets show example calls to the relevant NLP functions and the generated output:

{lang="lisp",linenos=off}
~~~~~~~~
KGN 39 > (setf text "Bill Clinton went to Canada")
"Bill Clinton went to Canada"

KGN 40 > (setf txtobj (kbnlp:make-text-object text))
#S(TEXT :URL "" :TITLE "" :SUMMARY "<no summary>" :CATEGORY-TAGS (("computers_microsoft.txt" 0.00641) ("religion_islam.txt" 0.00357)) :KEY-WORDS NIL :KEY-PHRASES NIL :HUMAN-NAMES ("Bill Clinton") :PLACE-NAMES ("Canada") :COMPANY-NAMES NIL :TEXT #("Bill" "Clinton" "went" "to" "Canada") :TAGS #("NNP" "NNP" "VBD" "TO" "NNP"))

KGN 41 > (kbnlp::text-human-names txtobj)
("Bill Clinton")

KGN 42 > 
(loop for key being the hash-keys of  (entity-uris:find-entities-in-text text)
  using (hash-value value)
  do (format t "key: ~S value: ~S~%" key value))
key: "people" value: (("Bill Clinton" "<http://dbpedia.org/resource/Bill_Clinton>"))
key: "countries" value: (("Canada" "<http://dbpedia.org/resource/Canada>"))
NIL
~~~~~~~~

The code using **loop** at the end of the last repl listing that prints keys and values of a hash table is from the [Common Lisp Cookbook web site](http://cl-cookbook.sourceforge.net/hashes.html) in the section "Traversing a Hash Table."

## Developing Low-Level SPARQL Utilities

I use the standard command line **curl** utility program with the Common Lisp package **uiop** to make HTML GET requests to the DBPedia public Knowledge Graph and the package **drakma** to url-encode parts of a query. The source code is in **src/kgn/sparql.lisp**. In lines 8, 24, 39, and 55 I use some caching code that we will look at later. The nested **replace-all** statements in lines 12-13 are a kluge to remove Unicode characters that occasionally caused runtime errors in the KGN application.

{lang="lisp",linenos=on}
~~~~~~~~
(in-package #:kgn)

(ql:quickload "cl-json")
(ql:quickload "drakma")

(defun sparql-dbpedia (query)
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
    (save-query-result-dbpedia query response)
    (ignore-errors
      (with-input-from-string
          (s response)
        (let ((json-as-list (json:decode-json s)))
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
~~~~~~~~

The code for replacing Unicode characters is messy but prevents problems later when we are using the query results in the example application.

The code **(json-as-list (json:decode-json s))** on line 28 converts a deeply nested JSON response to nested Common Lisp lists. You may want to print out the list to  better understand the **mapcar** expression on lines 31-35. There is no magic to writing expressions like this, in a repl I set **json-as-list** to the results of one query, and I spent a minute or two experimenting with the nested **mapcar** expression to get it to work with my test case.

The implementation for **sparql-ask-dbpedia** in lines 38-58 is simpler because we don't have to fully parse the returned SPARQL query results. A SPARQL **ask** type query returns a true/false answer to a query. We will use this to determine the types of entities in query text. While our NLP library identifies entity types, making additional **ask** queries to DBPedia to verify entity types will provide better automated results.

## Implementing the Caching Layer

While developing KGN and also using it as an end user, many SPARQL queries to DBPedia contain repeated entity names so it makes sense to write a caching layer.  We use a SQLite database "~/.kgn_cache.db" to store queries and responses.

The caching layer is implemented in the file **kgn/utils.lisp** and some of the relevant code is listed here:

{lang="lisp",linenos=on}
~~~~~~~~
;;; SqList caching for SPARQL queries:

(defvar *db-path* (pathname "~/.kgn_cache.db"))

(defun create-dbpedia ()
  (sqlite:with-open-database (d *db-path*)
    (ignore-errors
      (sqlite:execute-single d
        "CREATE TABLE dbpedia (query string  PRIMARY KEY ASC, result string)"))))

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
~~~~~~~~
                      
This caching layer greatly speeds up my own personal use of KGN. Without caching, queries that contain many entity references simply take too long to run. The UI for the KGN application has a menu option for clearing the local cache but I almost never use this option because growing a large cache that is tailored for the types of information I search for makes the entire system much more responsive.

## Utilities to Colorize SPARQL and Generated Output

When I first had the basic functionality of KGN working, I was disappointed by how the application looked as all black text on a white background. Every editor and IDE I use colorizes text in an appropriate way so I took advantage of the function **capi::write-string-with-properties** to (fairly) easily implement color hilting SPARQL queries.

The code in the following listing is in the file **kgn/colorize.lisp**. When I generate SPARQL queries to show the user I use the characters "@@" as placeholders for end of lines in the generated output. In line 5 I am ensuring that there are spaces around these characters so they get tokenized properly. In the loop starting at line 7 I process the tokens checking each one to see if it should have a color associated with it when it is written to the output stream.

{lang="lisp",linenos=on}
~~~~~~~~
(in-package #:kgn)

(defun colorize-sparql (s  &key (stream nil))
  (let ((tokens (tokenize-string-keep-uri
                    (replace-all s "@@" " @@ ")))
        in-var)
    (dolist (token tokens)
      (if (> (length token) 0)
          (if (or in-var (equal token "?"))
              (capi::write-string-with-properties
                token
                '(:highlight :compiler-warning-highlight)
                stream)
            (if (find token '("where" "select" "distinct" "option" "filter"
                              "FILTER" "OPTION" "DISTINCT"
                              "SELECT" "WHERE")
                      :test #'equal)
                (capi::write-string-with-properties 
                  token
                  '(:highlight :compiler-note-highlight)
                  stream)
              (if (equal (subseq token 0 1) "<")
                  (capi::write-string-with-properties
                    token
                    '(:highlight :bold)
                    stream)
                (if (equal token "@@")
                    (terpri stream)
                  (if (not (equal token "~")) (write-string token stream)))))))
      (if (equal token "?")
          (setf in-var t)
        (setf in-var nil))
      (if (and
           (not in-var)
           (not (equal token "?")))
          (write-string " " stream)))
    (terpri stream)))
~~~~~~~~

Here is an example call to function **colorize-sparql**:

{lang="sparql",linenos=off}
~~~~~~~~
KGN 25 > (colorize-sparql "select ?s ?p  where {@@  ?s ?p \"Microsoft\" } @@  FILTER (lang(?comment) = 'en')")
select ?s ?p where { 
 ?s ?p "Microsoft" } 
 FILTER ( lang ( ?comment ) = 'en' ) 
~~~~~~~~


## Text Utilities for Queries and Results

The utilities in the file **kgn/ui-text.lisp** contain no CAPI UI code but are used by the CAPI UI code. The function **display-entity-results** is passed an output stream that during repl development is passed as **t** to get output in the repl and in the application will be the output stream attached to a text pane. The argument **r-list** is a list of results where each result is a list containing a result title and a list of key/value pairs:

{lang="lisp",linenos=on}
~~~~~~~~
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
~~~~~~~~

The function **get-URIs-in-query** in lines 8-23 simply looks for URIs and saves them in a list.

In SPARQL queries, URIs are surround by angle brackets. The following code remove the brackets and embedded URIs. The function **remove-uris-from-query** simply looks for URIs in an input string and removes them:

{lang="lisp",linenos=on}
~~~~~~~~
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
~~~~~~~~

Here is a test:

{linenos=off}
~~~~~~~~
KGN 26 > 
(remove-uris-from-query
 "<http://dbpedia.org/resource/Bill_Gates> visited <http://dbpedia.org/resource/Apple_Inc.>")
" visited "
~~~~~~~~

Given a list of URIs, the following function makes multiple SPARQL queries to DBPedia to get more information using the function **get-name-and-description-for-uri** that we will look at later:

{lang="lisp",linenos=on}
~~~~~~~~
(defun handle-URIs-in-query (query)
  (let* ((uris (get-URIs-in-query query))
         (entity-names (map 'list #'get-name-and-description-for-uri uris)))
    (mapcar #'list uris (map 'list #'second entity-names))))
~~~~~~~~

The following repl show a call to **handle-URIs-in-query**:

{linenos=off}
~~~~~~~~
KGN 30 > (pprint (handle-URIs-in-query "<http://dbpedia.org/resource/Bill_Gates> visited <http://dbpedia.org/resource/Apple_Inc.>"))

(("<http://dbpedia.org/resource/Apple_Inc.>"
  "Apple Inc. is an American multinational technology company headquartered in Cupertino, California, that designs, develops, and sells consumer electronics, computer software, and online services. Its hardware products include the iPhone smartphone, the iPad tablet computer, the Mac personal computer, the iPod portable media player, the Apple Watch smartwatch, and the Apple TV digital media player. Apple's consumer software includes the macOS and iOS operating systems, the iTunes media player, the Safari web browser, and the iLife and iWork creativity and productivity suites. Its online services include the iTunes Store, the iOS App Store and Mac App Store, Apple Music, and iCloud.")
 ("<http://dbpedia.org/resource/Bill_Gates>"
  "William Henry \"Bill\" Gates III (born October 28, 1955) is an American business magnate, investor, author and philanthropist. In 1975, Gates and Paul Allen co-founded Microsoft, which became the world's largest PC software company. During his career at Microsoft, Gates held the positions of chairman, CEO and chief software architect, and was the largest individual shareholder until May 2014. Gates has authored and co-authored several books."))
~~~~~~~~

The function **get-entity-data-helper** processes the user's query and finds entities using both the NLP utilities from earlier in this book and by using SPARQL queries to DBPedia. Something new are calls to the function **updater** (lines 10-13, 17-20, and 29-31) that is defined as an optional argument. As we will see later, we will pass in a function value in the application that updates the progress bar at the bottom of the application window.

{lang="lisp",linenos=on}
~~~~~~~~
(defun get-entity-data-helper (original-query
                               &key
                               (message-stream t)
                               (updater nil))
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
          (if (not (member person people :test #'equal))
            (setf people (cons person people))))
        (let ((entity-list
               (list
                (cons :people
                      (append
                       (loop for person in people collect
                             (dbpedia-get-entities-by-name
                              person
                              "<http://dbpedia.org/ontology/Person>"
                              "<http://schema.org/Person>"
                              :message-stream message-stream))
                       (list people-uri)))
                (cons :countries
                      (append
                       (loop for country in countries collect
                             (dbpedia-get-entities-by-name
                              country
                              "<http://dbpedia.org/ontology/Country>"
                              "<http://schema.org/Country>"
                              :message-stream message-stream))
                       (list countries-uri)))
                (cons :cities
                      (append
                       (loop for city in cities collect
                             (dbpedia-get-entities-by-name
                              city
                              "<http://dbpedia.org/ontology/City>"
                              "<http://schema.org/City>"
                              :message-stream message-stream))
                       (list cities-uri)))
                (cons :places
                      (append
                       (loop for place in places collect
                             (dbpedia-get-entities-by-name
                              place
                              "<http://dbpedia.org/ontology/Place>"
                              "<http://schema.org/Place>"
                              :message-stream message-stream))
                       (list places-uri)))
                (cons :products
                      (loop for product in products collect
                            (dbpedia-get-entities-by-name
                             product
                             "<http://dbpedia.org/ontology/Product>"
                             "<http://schema.org/Product>"
                             :message-stream message-stream)))
                (cons :companies
                      (append
                       (loop for company in companies collect
                             (dbpedia-get-entities-by-name
                              company
                              "<http://dbpedia.org/ontology/Organization>"
                              "<http://schema.org/Organization>"
                              :message-stream message-stream))
                       (list companies-uri))))))
          (setf ret (prompt-selection-list entity-list))
          (format t "~%~%--------- ret:~%~%~S~%~%" ret)
          ret)))))
~~~~~~~~

This function presents a CAPI popup list selector to the user  so the following listed output depends on which possible entities are selected in this list. If you run the following repl example, you will see a popup window that will ask you to verify discovered entities; the user needs to check all discovered entities that are relevant to their interests.

{linenos=on}
~~~~~~~~
KGN 33 > (pprint (get-entity-data-helper "Bill Gates at Microsoft"))
((:PEOPLE
  (("<http://dbpedia.org/resource/Bill_Gates>"
    "William Henry \"Bill\" Gates III (born October 28, 1955) is an American business magnate, investor, author and philanthropist. In 1975, Gates and Paul Allen co-founded Microsoft, which became the world's largest PC software company. During his career at Microsoft, Gates held the positions of chairman, CEO and chief software architect, and was the largest individual shareholder until May 2014. Gates has authored and co-authored several books.")))
 (:COMPANIES
  (("<http://dbpedia.org/resource/Microsoft>"
    "Microsoft Corporation / 02C8ma 026Akr 0259 02CCs 0252ft, -ro 028A-, - 02CCs 0254 02D0ft/ (commonly referred to as Microsoft or MS) is an American multinational technology company headquartered in Redmond, Washington, that develops, manufactures, licenses, supports and sells computer software, consumer electronics and personal computers and services. Its best known software products are the Microsoft Windows line of operating systems, Microsoft Office office suite, and Internet Explorer and Edge web browsers. Its flagship hardware products are the Xbox video game consoles and the Microsoft Surface tablet lineup. As of 2011, it was the world's largest software maker by revenue, and one of the world's most valuable companies."))))
~~~~~~~~

The popup list in the last example looks like:

{width=90%}
![Popup list shows the user possible entity resolutions for each entity found in the input query. The user selects the resolved entities to use.](images/kgnpopup.png)

In this example there were two "Bill Gates" entities, one an early American frontiersman, the other the founder of Microsoft and I chose the latter person to continue finding information about.

After identifying all of the entities that the user intended, the function **entity-results->relationship-link** in the following listing is called to make additional SPARQL queries to discover possible relationships between these entities. This function is defined in the file **ui-text.lisp**.


{lang="lisp",linenos=on}
~~~~~~~~
(defun entity-results->relationship-links (results
                  &key (message-stream t) (updater nil))
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
                (setf relationship-statements
                  (cons (list e1 e2 x) relationship-statements)))
              (dolist (x l2)
                (print (list "x l2:" x))
                (setf relationship-statements
                  (cons (list e2 e1 x) relationship-statements)))))))
    (setf relationship-statements
      (remove-duplicates relationship-statements :test #'equal))
    ;;(terpri message-stream)
    (capi::write-string-with-properties
      "DISCOVERED RELATIONSHIP LINKS:"
      '(:highlight :compiler-warning-highlight) message-stream)
    (terpri message-stream) (terpri message-stream)
    (dolist (rs relationship-statements)
      (format message-stream "~43A" (first rs))
      (capi::write-string-with-properties
        sep
        '(:highlight :compiler-warning-highlight) message-stream)
      (format message-stream "~43A" (third rs))
      (capi::write-string-with-properties
	      sep
	      '(:highlight :compiler-warning-highlight) message-stream)
      (format message-stream "~A" (second rs))
      (terpri message-stream))
    relationship-statements))
~~~~~~~~

In the following repl listing we create some test data of the same form as we get from calling function **get-entity-data-helper** seen in a previous listing and try calling **entity-results->relationship-links** with this data:

{linenos=off}
~~~~~~~~
KGN 36 > (setf results '((:PEOPLE
  (("<http://dbpedia.org/resource/Bill_Gates>"
    "William Henry \"Bill\" Gates III (born October 28, 1955) is an American business magnate, investor, author and philanthropist. In 1975, Gates and Paul Allen co-founded Microsoft, which became the world's largest PC software company. During his career at Microsoft, Gates held the positions of chairman, CEO and chief software architect, and was the largest individual shareholder until May 2014. Gates has authored and co-authored several books.")))
 (:COMPANIES
  (("<http://dbpedia.org/resource/Microsoft>"
    "Microsoft Corporation / 02C8ma 026Akr 0259 02CCs 0252ft, -ro 028A-, - 02CCs 0254 02D0ft/ (commonly referred to as Microsoft or MS) is an American multinational technology company headquartered in Redmond, Washington, that develops, manufactures, licenses, supports and sells computer software, consumer electronics and personal computers and services. Its best known software products are the Microsoft Windows line of operating systems, Microsoft Office office suite, and Internet Explorer and Edge web browsers. Its flagship hardware products are the Xbox video game consoles and the Microsoft Surface tablet lineup. As of 2011, it was the world's largest software maker by revenue, and one of the world's most valuable companies.")))))
KGN 37 > (pprint (entity-results->relationship-links results))
(("<http://dbpedia.org/resource/Bill_Gates>"
  "<http://dbpedia.org/resource/Microsoft>"
  "<http://dbpedia.org/ontology/board>")
 ("<http://dbpedia.org/resource/Microsoft>"
  "<http://dbpedia.org/resource/Bill_Gates>"
  "<http://dbpedia.org/property/founders>")
 ("<http://dbpedia.org/resource/Microsoft>"
  "<http://dbpedia.org/resource/Bill_Gates>"
  "<http://dbpedia.org/ontology/keyPerson>"))
~~~~~~~~


## Using LispWorks CAPI UI Toolkit

You can use the free LispWorks Personal Edition for running KGN. Using other Common Lisp implementations like Clozure-CL and SBCL will not work because the CAPI user interface library is proprietary to LispWorks. I would like to direct you to three online resources for learning CAPI:

- [LispWorks' main web [age introducing CAPI](http://www.lispworks.com/products/capi.html)
- [LispWorks' comprehensive CAPI documentation](http://www.lispworks.com/products/capi.html) for LispWorks version 7.1
- An older web site (last updated in 2011 but I find it useful for ideas): [CAPI Cookbook](http://capi.plasticki.com/show?O4)

I am not going to spend too much time in this chapter explaining my CAPI-based code. If you use LispWorks (either the free Personal or the Professional editions) you are likely to use CAPI and spending time on the official documentation and especially the included example programs is strongly recommended.

In the next section I will review the KGN specific application parts of the CAPI-based UI.

## Writing Utilities for the UI

The CAPI user interface code is in the file **src/kgn/gui.lisp** with some UI code in **options-pane.lisp** and **kgn.lisp**.

When printing results in the bottom Results Pane of the KGN application, I like to highlight the first line of each result using this function (first function in **kgn.lisp**):

{lang="lisp",linenos=on}
~~~~~~~~
(defun pprint-results (results &key (stream t))
  (dolist (result (car results))
    (terpri stream)
    (capi::write-string-with-properties 
     (format nil  "~A:" (first result))
     '(:highlight :compiler-warning-highlight) stream)
    (format stream " ~A~%" (second result))))
~~~~~~~~

I default the value for the input named variable **stream** to **t** so during development in a repl the output of this function goes to standard output. In the KGN app, I get an output stream for the bottom results pane in the user interface and pass that as the value for **stream** so output is directly written to the results pane.

CAPI allows you to define your own text highlight values. I use built-in ones like **:compiler-warning-highlight** that are always available to CAPI applications.

The file **kgn.lisp** defines several other utility functions including a utility that makes multiple SPARQL queries to get a name and description of an entity URI that removes end of line markers "@@" from a SPARQL query for fetching entity data, makes the query and extracts results for display:

{lang="lisp",linenos=on}
~~~~~~~~
(defun get-name-and-description-for-uri (uri)
  (let* ((sparql
          (replace-all
           (format nil "select distinct ?name ?comment { @@ ~
                         values ?nameProperty {<http://www.w3.org/2000/01/rdf-schema#label> <http://xmlns.com/foaf/0.1/name> } . @@ ~
                         ~A ?nameProperty ?name . @@ ~
                         ~A <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment  . FILTER  (lang(?comment) = 'en') . @@ ~
                     } LIMIT 1" uri uri)
           "@@" " "))
         (results (sparql-dbpedia sparql)))
    (list 
      (second (assoc :name (car results))) 
      (second (assoc :comment (car results))))))
~~~~~~~~
    
There are several other SPARQL query utility functions in the file **kgn.lisp** that I will not discuss but they follow a similar pattern of using specific SPARQL queries to fetch information from DBPedia.

At the top of the file **gui.lisp** I set three parameters for the width of the application window and a global flag used to toggle on and off showing the info-pane-grapher that you saw in the screen shot at the beginning of this chapter and that is also shown below:

{lang="lisp",linenos=on}
~~~~~~~~
(defvar *width* 1370)
(defvar *best-width* 1020)
(defvar *show-info-pane* t)
~~~~~~~~

{width=70%}
![UI for info-pane-grapher](images/info-pane-browser.png)

Since I just mentioned the **info-pane-grapher** this is a good time to digress to its implementation. This is in a different package and you will find the source code in **src/lw-grapher/info-pane-grapher.lisp**. I used the graph layout algorithm from [ISI-Grapher Manual (by Gabriel Robbins)](http://www.cs.virginia.edu/~robins/papers/The_ISI_Grapher_Manual.pdf). There is another utility in **src/lw-grapher/lw-grapher.lisp** that also displays a graph without mouse support and an attached information pane that is not used here but you might prefer it for reuse in your projects if you don't need mouse interactions.

The graph nodes are derived from the class **capi:pinboard-object**:
 
{lang="lisp",linenos=on}
~~~~~~~~
(defclass text-node (capi:pinboard-object)
  ((text :initarg :text :reader text-node-text)
   (string-x-offset :accessor text-node-string-x-offset)
   (string-y-offset :accessor text-node-string-y-offset)))
~~~~~~~~

I customized how my graph nodes are drawn in a graph pane (this is derived from LispWorks example code):

{lang="lisp",linenos=on}
~~~~~~~~
(defmethod capi:draw-pinboard-object (pinboard (self text-node)
                                               &key &allow-other-keys)
  (multiple-value-bind (X Y  width height)
      (capi:static-layout-child-geometry self)
    (let* ((half-width  (floor (1- width)  2))
           (half-height (floor (1- height) 2))
           (circle-x (+ X half-width))
           (circle-y (+ Y half-height))
           (background :white)
           (foreground (if background
                           :black
                         (capi:simple-pane-foreground pinboard)))
           (text (text-node-text self)))
        (gp:draw-ellipse pinboard
                           circle-x circle-y
                           half-width half-height
                           :filled t
                           :foreground background)
        (gp:draw-ellipse pinboard
                         circle-x circle-y
                         half-width half-height
                         :foreground foreground)
        (gp:draw-string pinboard
                        text
                        (+ X (text-node-string-x-offset self))
                        (+ Y (text-node-string-y-offset self))
                        :foreground foreground))))
~~~~~~~~

Most of the work is done in the graph layout method that uses Gabriel Robbins' algorithm. Here I just show the signature and we won't go into implementation. If you are interested in modifying the layout code, I include a screen shot from ISI-Grapher manual showing the algorithm in a single page, see the file **src/lw-grapher/Algorithm from ISI-Grapher Manual.png**.
 
The following code snippets shows the method signature for the layout algorithm function in the file **src/lw-grapher/grapher.lisp**. I also include the call to **capi:graph-pane-nodes** that is the CLOS reader method for getting the list of node objects in a graph pane:
 
{lang="lisp",linenos=on}
~~~~~~~~
(defun graph-layout (self &key force)
  (declare (ignore force))
  (let* ((nodes (capi:graph-pane-nodes self))
    ...
~~~~~~~~

The CAPI graph node model uses a function that is passed a node object and returns a list this node's child node objects. There are several examples of this in the CAPI graph examples that are included with LispWorks (see the CAPI documentation).

In **src/lw-grapher/lw-grapher.lisp** I wrote a function that builds a graph layout and instead of passing in a "return children" function I found it more convenient to wrap this process, accepting a list of graph nodes and graph edges as function arguments:

{lang="lisp",linenos=on}
~~~~~~~~
(in-package :lw-grapher)

;; A Grapher (using the layout algorithm from the ISI-Grapher
;; user guide) with an info panel

(defun make-info-panel-grapher (h-root-name-list h-edge-list
                                h-callback-function-click
                                h-callback-function-shift-click)
  (let (edges roots last-selected-node node-callback-click
	  node-callback-click-shift output-pane)
    (labels
        ((handle-mouse-click-on-pane (pane x y)
           (ignore-errors
             (let ((object (capi:pinboard-object-at-position pane x y)))
               (if object
                   (let ()
                     (if last-selected-node
                         (capi:unhighlight-pinboard-object pane
                             last-selected-node))
                     (setf last-selected-node object)
                     (capi:highlight-pinboard-object pane object)
                     (let ((c-stream (collector-pane-stream output-pane))) 
                       (format c-stream
                         (funcall node-callback-click
                           (text-node-full-text object)))
                       (terpri c-stream)))))))
         (handle-mouse-click-shift-on-pane (pane x y)
           (ignore-errors
             (let ((object
                    (capi:pinboard-object-at-position pane x y)))
               (if object
                   (let ()
                     (if last-selected-node
                         (capi:unhighlight-pinboard-object
                           pane last-selected-node))
                     (setf last-selected-node object)
                     (capi:highlight-pinboard-object pane object)
                     (let ((c-stream
                             (collector-pane-stream output-pane)))
                       (format c-stream
                         (funcall node-callback-click-shift
                           (text-node-full-text object)))
                       (terpri c-stream)))))))
         
         (info-panel-node-children-helper (node-text)
           (let (ret)
             (dolist (e edges)
               (if (equal (first e) node-text)
                   (setf ret (cons (second e) ret))))
             (reverse ret)))
         
         (make-info-panel-grapher-helper
           (root-name-list edge-list callback-function-click
            callback-function-click-shift)
           ;; example: root-name-list: '("n1") edge-list:
           ;;   '(("n1" "n2") ("n1" "n3"))
           (setf edges edge-list
                 roots root-name-list
                 node-callback-click callback-function-click
                 node-callback-click-shift callback-function-click-shift)
           (capi:contain 

            (make-instance
             'column-layout
             :title "Entity Browser"
             :description
             (list
              (make-instance 'capi:graph-pane
                             :min-height 330
                             :max-height 420
                             :roots roots
                             :layout-function 'graph-layout
                             :children-function #'info-panel-node-children-helper
                             :edge-pane-function 
                             #'(lambda(self from to)
                                 (declare (ignore self))
                                 (let ((prop-name ""))
                                   (dolist (edge edge-list)
                                     (if (and
                                          (equal from (first edge))
                                          (equal to (second edge)))
                                         (if (and (> (length edge) 2) (third edge))
                                             (let ((last-index
                                                     (search
                                                      "/" (third edge) 
                                                      :from-end t)))
                                               (if last-index
                                                   (setf prop-name 
                                                    (subseq (third edge) 
                                                    (1+ last-index)))
                                                 (setf prop-name (third edge)))))))
                                   (make-instance 
                                    'capi:labelled-arrow-pinboard-object
                                    :data (format nil "~A" prop-name))))
                             :node-pinboard-class 'text-node
                             :input-model `(((:button-1 :release)
                                             ,#'(lambda (pane x y)
                                                  (handle-mouse-click-on-pane 
                                                    pane x y)))
                                            ((:button-1 :release :shift) ;; :press)
                                             ,#'(lambda (pane x y)
                                                  (handle-mouse-click-shift-on-pane 
                                                    pane x y))))
                             :node-pane-function 'make-text-node)
              (setf
               output-pane
               (make-instance 'capi:collector-pane
                              :min-height 130
                              :max-height 220
                              :title "Message collection pane"
                              :text "..."
                              :vertical-scroll t
                              :horizontal-scroll t))))
            :title 
            "Info Pane Browser: mouse click for info, mouse click + shift for web browser"
            
            :best-width 550 :best-height 450)))
      (make-info-panel-grapher-helper h-root-name-list
        h-edge-list h-callback-function-click
        h-callback-function-shift-click))))
  ~~~~~~~~


## Writing the UI

Returning to the file **src/kgn/gui.lisp**, we need to implement callback functions for handling mouse clicks on the **info-pane-panel**, showing the options popup panel, and handling the callback when the user wants to delete the local SQLite query cache:

{lang="lisp",linenos=on}
~~~~~~~~
(defun test-callback-click (selected-node-name)
  (ignore-errors
    (format nil "* user clicked on node: ~A~%" selected-node-name)))

(defun test-callback-click-shift (selected-node-name)
  (ignore-errors
    (if (equal (subseq selected-node-name 0 5) "<http")
        (trivial-open-browser:open-browser 
         (subseq selected-node-name 1
           (- (length selected-node-name) 1))))
    (format
      nil
      "* user shift-clicked on node: ~A - OPEN WEB BROWSER~%"
      selected-node-name)))
  
(defun cache-callback (&rest x) (declare (ignore x))
  (if *USE-CACHING*
      (capi:display
        (make-instance 'options-panel-interface))))

(defun website-callback (&rest x)
  (declare (ignore x))
  (trivial-open-browser:open-browser
    "http://www.knowledgegraphnavigator.com/"))
~~~~~~~~

In lines 8-10 I am using a third party package **trivial-open-browser:open-browser** to open the default browser on your laptop. URIs in KGN have angle bracket characters around the URI so here we remove these characters. I also use this same function in lines 21-24 to show the user a web site that I built for this example application.

Again from **gui.lisp**, the following listing shows how to define the CAPI user interface and I refer you to the CAPI documentation for details:

{lang="lisp",linenos=on}
~~~~~~~~
(capi:define-interface kgn-interface ()
  ()
  (:menus
   (action-menu 
    "Actions"
    (
     ("Copy generated SPARQL to clipboard"
      :callback
      #'(lambda (&rest x) (declare (ignore x))
          (let ((messages (capi:editor-pane-text text-pane2)))
            (capi::set-clipboard text-pane2
              (format nil "---- Generated SPARQL and comments:~%~%~A~%~%" messages)
              nil))))
     ("Copy results to clipboard"
      :callback
      #'(lambda (&rest x) (declare (ignore x))
          (let ((results (capi:editor-pane-text text-pane3)))
            (capi::set-clipboard text-pane2
              (format nil "---- Results:~%~%~A~%" results) nil))))
     ("Copy generated SPARQL and results to clipboard"
      :callback
      #'(lambda (&rest x) (declare (ignore x))
          (let ((messages (capi:editor-pane-text text-pane2))
                (results (capi:editor-pane-text text-pane3)))
            (capi::set-clipboard
             text-pane2
             (format nil
               "---- Generated SPARQL and comments:~%~%~A~%~%---- Results:~%~%~A~%"
               messages results) nil))))
     ("Visit Knowledge Graph Navigator Web Site" :callback 'website-callback)
     ("Clear query cache" :callback 'cache-callback)
     ((if *show-info-pane*
          "Stop showing Grapher window for new results"
        "Start showing Grapher window for new results")
      :callback 'toggle-grapher-visibility)
     )))
  (:menu-bar action-menu)
  (:panes
   (text-pane1
    capi:text-input-pane
    :text (nth (random (length *examples*)) *examples*)
    :title "Query"
    :min-height 80
    :max-height 100
    :max-width *width*
    ;;:min-width (- *width* 480)
    :width *best-width*
    :callback 'start-progress-bar-test-from-background-thread)

   (progress-bar
    capi:progress-bar
    :start 0
    :end 100
    )

   (text-pane2
    capi:collector-pane
    :font "Courier"
    :min-height 210
    :max-height 250
    :title "Generated SPARQL queries to get results"
    :text "Note: to answer queries, this app makes multipe SPARQL queries to DBPedia. These SPARQL queries will be shown here."
    :vertical-scroll t
    :create-callback #'(lambda (&rest x)
                         (declare (ignore x))
                         (setf (capi:editor-pane-text text-pane2) *pane2-message*))
    :max-width *width*
    :width *best-width*
    :horizontal-scroll t)

   (text-pane3
    capi:collector-pane ;; capi:display-pane ;; capi:text-input-pane
    :text *pane3-message*
    :font "Courier"
    :line-wrap-marker nil
    :wrap-style :split-on-space
    :vertical-scroll :with-bar
    :title "Results"
    :horizontal-scroll t
    :min-height 220
    :width *best-width*
    :create-callback #'(lambda (&rest x)
                         (declare (ignore x))
                         (setf (capi:editor-pane-text text-pane3) *pane3-message*))
    :max-height 240
    :max-width *width*)
   (info
    capi:title-pane
    :text "Use natural language queries to generate SPARQL"))
  (:layouts
   (main-layout
    capi:grid-layout
    '(nil info
      nil text-pane1
      nil text-pane2
      nil text-pane3
      nil progress-bar)
     :x-ratios '(1 99)
    :has-title-column-p t))
  (:default-initargs
   :layout 'main-layout
   :title "Knowledge Graph Navigator"
   :best-width *best-width*
   :max-width *width*))
~~~~~~~~

I showed you how to run the KGN example application earlier and I suggest that you leave the application open when reading through the user interface code.

For most of the development of KGN, the code layout and control flow was fairly simple. After the application was complete however, I noticed a bad user interface problem: making many calls to the DBPedia service took time and the application and except for streaming output to the generated SPARQL pane the application does nothing for a while which could confuse users. I decided to add a progress bar at the bottom of the main window and extracted much of the query processing functionality to a work thread, as implemented in the following listing, and pass a "update progress bar" callback function to many of the helper functions that create the SPARQL queries, make the web calls, and process the results. This callback function moves the progress bar. This complexity makes the KGN code is not as good a book example, but makes the application much better. The following function is derived from a multi-processing LispWorks example program. The local function **update-progress-bar** defined in the special operator **flet** in lines 4-8 is the function **updater** passed into functions we have seen earlier. This function updates the progress bar and is called during long running function calls. **flet** is like a **let** that additionally allows definitions of functions that inherit the local content of any variables defined in the **flet**.

{lang="lisp",linenos=on}
~~~~~~~~
(defun start-progress-bar-test-from-background-thread (query-text self)
  (with-slots (text-pane2 text-pane3  progress-bar) self
    (print text-pane2)
    (flet ((update-progress-bar (percent)
             (capi:execute-with-interface
              self
              #'(lambda ()
                  (setf (capi:range-slug-start progress-bar) percent)))))
      (mp:process-run-function "progress-bar-test-from-background-thread"
                               '()
                               'run-and-monitor-progress-background-thread
                               #'update-progress-bar
                               query-text text-pane2 text-pane3
                             ))))

(defvar *percent*)

(defun run-and-monitor-progress-background-thread 
                       (updater text text-pane2 text-pane3)
  (setf *percent* 0)
  (unwind-protect
      (setf (capi:editor-pane-text text-pane2) "")
    (setf (capi:editor-pane-text text-pane3) "")
    ;;(capi:display-message "done")
    (let ((message-stream (collector-pane-stream text-pane2))
          (results-stream (collector-pane-stream text-pane3)))
      (format message-stream "# Starting to process query....~%")
      (format results-stream *pane3-message*)
      (let ((user-selections 
              (get-entity-data-helper text 
                 :updater updater 
                 :message-stream message-stream)))
        (setf *percent* (+ *percent* 2))
        (funcall updater *percent*)
        (setf (capi:editor-pane-text text-pane3) "")
        (dolist (ev user-selections)
          (if (> (length (cadr ev)) 0)
              (let ()
                (terpri results-stream)
                (capi::write-string-with-properties
                 (format nil "- - - ENTITY TYPE: ~A - - -" (car ev))
                 '(:highlight :compiler-error-highlight) results-stream)
                (terpri results-stream)
                (dolist (uri (cadr ev))
                  (setf uri (car uri))
                  (case (car ev)
                    (:people
                     (pprint-results 
                      (dbpedia-get-person-detail  uri :message-stream message-stream)
                      :stream results-stream))
                    (:companies
                     (pprint-results 
                      (dbpedia-get-company-detail uri :message-stream message-stream)
                      :stream results-stream))
                    (:countries
                     (pprint-results
                      (dbpedia-get-country-detail uri :message-stream message-stream)
                      :stream results-stream))
                    (:cities
                     (pprint-results
                      (dbpedia-get-city-detail    uri :message-stream message-stream)
                      :stream results-stream))
                    (:products
                     (pprint-results
                      (dbpedia-get-product-detail uri :message-stream message-stream)
                      :stream results-stream))))))
          (setf *percent* (+ *percent* 1))
          (funcall updater *percent*))

        (let (links x)
          (dolist (ev user-selections)
            (dolist (uri (second ev))
              (setf uri (car uri))
              (if (> (length ev) 2)
                  (setf x (caddr ev)))
              (setf links (cons (list (symbol-name (first ev)) uri x) links))
              (setf *percent* (+ *percent* 1))
              (funcall updater *percent*)))

         (setf
           links
           (append
            links
            (entity-results->relationship-links
             user-selections
             :message-stream message-stream
             :updater updater)))
          (setf *percent* (+ *percent* 2))
          (funcall updater *percent*)

          (if
            *show-info-pane*
              (lw-grapher:make-info-panel-grapher
                '("PEOPLE" "COMPANIES" "COUNTRIES" "CITIES"
                  "PRODUCTS" "PLACES")                                   
                links 'test-callback-click
                'test-callback-click-shift)))))
    (funcall updater 0)))
~~~~~~~~

We call the callback function **updater** at the end to remove the progress bar to let the user know that they can now enter another query.

If you have not already done so I hope you will take some time to download the LispWorks Personal Edition and try this application.

## Wrap-up

This is a long example application for a book so I did not discuss all of the code in the project. If you enjoy running and experimenting with this example and want to modify it for your own projects then I hope that I provided a sufficient road map for you to do so.

I got the idea for the KGN application because I was spending quite a bit of time manually setting up SPARQL queries for DBPedia (and other public sources like WikiData) and I wanted to experiment with partially automating this process. I wrote the CAPI user interface for fun since this example application could have had similar functionality as a command line tool. In fact, my first cut implementation was a command line tool with the user interface in the file **ui-text** that we looked at earlier. I decided to remove the command line interface and replace it using CAPI.

Most of the Common Lisp development I do has no user interface or implements a web application. When I do need to write an application with a user interface, the LispWorks CAPI library makes writing user interfaces fairly easy to do.

If you are using an open source Common Lisp like SBCL or CCL and you want to add a user interface then you might want to also try [LTK](http://www.peter-herth.de/ltk/) and [McClim](https://www.cliki.net/McCLIM). McClim works well on Linux and also works on macOS with XQuartz but with fuzzy fonts. I also like [Radiance](https://github.com/Shirakumo/radiance) that spawns a web browser so you can package web applications as desktop applications.

If you are using CCL (Clojure Common Lisp) on macOS you can try the supported **COCOA-APPLICATION** package. This is only recommended if you already know the Cocoa APIs, otherwise this route has a very steep learning curve.
