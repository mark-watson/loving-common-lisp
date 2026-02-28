# Information Gathering {#information_gathering}

This chapter covers information gathering on the web using data sources and general techniques that I have found useful. When I was planning this new book edition I had intended to also cover some basics for using the Semantic Web from Common Lisp, basically distilling some of the data from my previous book "Practical Semantic Web and Linked Data Applications, Common Lisp Edition" published in 2011. However since a [free PDF is now available for that book](http://markwatson.com/#books/) I decided to just refer you to my previous work if you are interested in the Semantic Web and Linked Data. You can also find the Java edition of this previous book on my web site.

Gathering information from the web in realtime has some real advantages:

- You don't need to worry about storing data locally.
- Information is up to date (depending on which web data resources you choose to use).

There are also a few things to consider:

- Data on the web may have legal restrictions on its use so be sure to read the terms and conditions on web sites that you would like to use.
- Authorship and validity of data may be questionable.


## DBPedia Lookup Service

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
(ql:quickload :drakma)
(ql:quickload :babel)
(ql:quickload :s-xml)

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
           "http://lookup.dbpedia.org/api/search.asmx/KeywordSearch?QueryString="
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
    ;; (print (list "raw response body as XML:" response-body))
    ;;(print (list ("status:" response-status "headers:" response-headers)))
    (setf xml
          (s-xml:parse-xml-string
           (babel:octets-to-string response-body)))
    (dolist (r (cdr xml))
      ;; assumption: data is returned in the order:
      ;;   1. label
      ;;   2. DBPedia URI for more information
      ;;   3. description
      (push
       (make-dbpedia-data
        :uri (cadr (nth 2 r))
        :label (cadr (nth 1 r))
        :description
        (string-trim
         '(#\Space #\NewLine #\Tab)
         (cadr (nth 3 r))))
       ret))
    (reverse ret)))

;; (dbpedia-lookup "berlin")
~~~~~~~~

I am only capturing the attributes for DBPedia URI, label and description in this example code. If you uncomment line 41 and look at the entire response body from the call to DBPedia Lookup, you can see other attributes that you might want to capture in your applications.

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


## Web Spiders

When you write web spiders to collect data from the web there are two things to consider:

- Make sure you read the terms of service for web sites whose data you want to use. I have found that calling or emailing web site owners explaining how I want to use the data on their site usually works to get permission.
- Make sure you don't access a site too quickly. It is polite to wait a second or two between fetching pages and other assets from a web site.

We have already used the Drakma web client library in this book. See the files **src/dbpedia/dbpedia-lookup.lisp** (covered in the last section) and **src/solr_examples/solr-client.lisp** (covered in the [Chapter on NoSQL](#nosql_chapter)). Paul Nathan has written library using Drakma to crawl a web site with an example to print out links as they are found. His code is available under the AGPL license at [articulate-lisp.com/src/web-trotter.lisp](http://articulate-lisp.com/examples/trotter.html) and I recommend that as a starting point.

I find it is sometimes easier during development to make local copies of a web site so that I don't have to use excess resources from web site hosts. Assuming that you have the **wget** utility installed, you can mirror a site like this:

~~~~~~~~
wget -m -w 2 http://knowledgebooks.com/
wget -mk -w 2 http://knowledgebooks.com/
~~~~~~~~

Both of these examples have a two-second delay between HTTP requests for resources. The option **-m** indicates to recursively follow all links on the web site. The **-w** **2** option delays for two seconds between requests. The option **-mk** converts URI references to local file references on your local mirror. The second example on line 2 is more convenient.

We covered reading from local files in the [Chapter on Input and Output](#input_output). One trick I use is to simply concatenate all web pages into one file. Assuming that you created a local mirror of a web site, cd to the top level directory and use something like this:

~~~~~~~~
cd knowledgebooks.com
cat *.html */*.html > ../web_site.html
~~~~~~~~

You can then open the file, search for text in in **p**, **div**, **h1**, etc. HTML elements to process an entire web site as one file.

## Using Apache Nutch

[Apache Nutch](https://nutch.apache.org/), like Solr, is built on Lucene search technology. I use Nutch as a "search engine in a box" when I need to spider web sites and I want a local copy with a good search index.

Nutch handles a different developer's use case over Solr which we covered in the [Chapter on NoSQL](#nosql_chapter). As we saw, Solr is an effective tool for indexing and searching structured data as documents. With very little setup, Nutch can be set up to automatically keep an up to date index of a list of web sites, and optionally follow links to some desired depth from these "seed" web sites.

You can use the same Common Lisp client code that we used for Solr with one exception; you will need to change the root URI for the search service to:

     http://localhost:8080/opensearch?query=

So the modified client code **src/solr_examples/solr-client.lisp** needs one line changed:

{lang="lisp",linenos=on}
~~~~~~~~
(defun do-search (&rest terms)
  (let ((query-string (format nil "~{~A~^+AND+~}" terms)))
   (cl-json:decode-json-from-string
     (drakma:http-request
       (concatenate 
        'string
        "http://localhost:8080/opensearch?query="
        query-string
        "&wt=json")))))
~~~~~~~~

Early versions of Nutch were very simple to install and configure. Later versions of Nutch have been more complex, more performant, and have more services, but it will take you longer to get set up than earlier versions. If you just want to experiment with Nutch, you might want to start with an earlier version.

The [OpenSearch.org](http://www.opensearch.org/Home) web site contains many public OpenSearch services that you might want to try. If you want to modify the example client code in **src/solr-client.lisp** a good start is OpenSearch services that return JSON data and [OpenSearch Community JSON formats web page](http://www.opensearch.org/Community/JSON_Formats) is a good place to start. Some of the services on this web page like the New York Times service require that you sign up for a developer's API key.

When I start writing an application that requires web data (no matter which programming language I am using) I start by finding services that may provide the type of data I need and do my initial development with a web browser with plugin support to nicely format XML and JSON data. I do a lot of exploring and take a lot of notes before I write any code.


## Wrap Up

I tried to provide some examples and advice in this short chapter to show you that even though other languages like Ruby and Python have more libraries and tools for gathering information from the web, Common Lisp has good libraries for information gathering also and they are easily used via Quicklisp.


