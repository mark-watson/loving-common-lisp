# Network Programming  {#network_prog}

Distributed computing is pervasive: you need to look no further than the World Wide Web, Internet chat, etc. Of course, as a Lisp programmer, you will want to do at least some of your network programming in Lisp! The previous editions of this book provided low level socket network programming examples. I decided that for this new edition, I would remove those examples and instead encourage you to "move further up the food chain" and work at a higher level of abstraction that makes sense for the projects you will likely be developing. Starting in the 1980s, a lot of my work entailed low level socket programming for distributed networked applications. As I write this, it is 2013, and there are better ways to structure distributed applications.

Specifically, since many of the examples later in this book fetch information from the web and linked data sources, we will start be learning how to use Edi Weitz's [Drakma HTTP client library](http://weitz.de/drakma/). In order to have a complete client server example we will also look briefly at Edi Weitz's [Hunchentoot web server](http://weitz.de/hunchentoot/) that uses JSON as a data serialization format. I used to use XML for data serialization but JSON has many advantages: easier for a human to read and it plays nicely with Javascript code and some data stores like Postgres (new in versions 9.x), MongoDB, and CouchDB that support JSON as a native data format.

The code snippets in the first two sections of this chapter are derived from examples in the Drackma and Hunchentoot documentation.


## An introduction to Drakma

Edi Weitz's [Drakma library](http://weitz.de/drakma/) supports fetching data via HTTP requests. As you can see in the Drakma documentation, you can use this library for authenticated HTTP requests (i.e., allow you to access web sites that require a login), support HTTP GET and PUT operations, and deal with cookies. The top level API that we will use is **drakma:http-request** that returns multiple values. In the following example, I want only the first three values, and ignore the others like the original URI that was fetched and an IO stream object. We use the built-in Common Lisp macro **multiple-value-setq**:

~~~~~~~~
* (ql:quickload :drakma)
* (multiple-value-setq
    (data http-response-code headers)
    (drakma:http-request "http://markwatson.com"))
~~~~~~~~

I manually formatted the last statement I entered in the last repl listing and I will continue to manually edit the repl listings in the rest of this book to make them more easily readable.

The following shows some of the data bound to the variables **data**, **http-response-code**, and **headers**:

~~~~~~~~
* data

"<!DOCTYPE html>
<html>
  <head>
	<title>Mark Watson: Consultant and Author</title>
~~~~~~~~

The value of **http-response-code** is 200 which means that there were no errors:

~~~~~~~~
* http-response-code

200
~~~~~~~~

The HTTP response headers will be useful in many applications; for fetching the home page of my web site the headers are:

~~~~~~~~
* headers

((:SERVER . "nginx/1.1.19")
 (:DATE . "Fri, 05 Jul 2013 15:18:27 GMT")
 (:CONTENT-TYPE . "text/html; charset=utf-8")
 (:TRANSFER-ENCODING . "chunked")
 (:CONNECTION . "close")
 (:SET-COOKIE
   .
   "ring-session=cec5d7ba-e4da-4bf4-b05e-aff670e0dd10;Path=/"))
~~~~~~~~

We will use Drakma later in this book for several examples. In the next section we will write a web app using Hunchentoot and test it with a Drakma client.



## An introduction to Hunchentoot

Edi Weitz's [Hunchentoot project](http://weitz.de/hunchentoot/) is a flexible library for writing web applications and web services. We will also use Edi's CL-WHO library in this section for generating HTML from Lisp code. Hunchentoot will be installed the first time you quick load it in the example code for this section:

~~~~~~~~
(ql:quickload "hunchentoot")
~~~~~~~~

I will use only [easy handler framework](http://weitz.de/hunchentoot/#easy-handlers) in the Hunchentoot examples in this section. I leave it to you to read the [documentation on using custom acceptors](http://weitz.de/hunchentoot/#acceptors) after you experiment with the examples in this section.

The following code will work for both multi-threading installations of SBCL and single thread installations (e.g., some default installations of SBCL on OS X):

{lang="lisp",linenos=on}
~~~~~~~~
(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(in-package :cl-user)
(defpackage hdemo
  (:use :cl
        :cl-who
        :hunchentoot))
(in-package :hdemo)

(defvar *h* (make-instance 'easy-acceptor :port 3000))

;; define a handler with the arbitrary name my-greetings:

(define-easy-handler (my-greetings :uri "/hello") (name)
  (setf (hunchentoot:content-type*) "text/html")
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "hunchentoot test"))
     (:body
      (:h1 "hunchentoot form demo")
      (:form
       :method :post
       (:input :type :text
	       :name "name"
	       :value name)
       (:input :type :submit :value "Submit your name"))
      (:p "Hello " (str name))))))

(hunchentoot:start *h*)
~~~~~~~~

In lines 5 through 9 we create an use a new package that includes support for generating HTML in Lisp code (CL-WHO) and the Hunchentoot library). On line 11 we create an instance of an easy acceptor on port 3000 that provides useful default behaviors for providing HTTP services.

The Hunchentoot macro **define-easy-handler** is used in lines 15 through 28 to define an HTTP request handler and add it to the easy acceptor instance. The first argument, **my-greetings** in this example, is an arbitrary name and the keyword **:uri** argument provides a URL pattern that the easy acceptor server object uses to route requests to this handler. For example, when you run this example on your computer, this URL routing pattern would handle requests like:

    http://localhost:3000/hello

In lines 17 through 28 we are using the CL-WHO library to generate HTML for a web page. As you might guess, **:html** generates the outer \<html\>\</html\> tags for a web page. Line 19 would generate HTML like:

~~~~~~~~
  <head>
	<title>hunchentoot test</title>
  </head>
~~~~~~~~

Lines 22 through 27 generate an HTML input form and line 28 displays any value generated when the user entered text in the input filed and clicked the submit button. Notice the definition of the argument **name** in line 1 in the definition of the easy handler. If the argument **name** is not defined, the **nil** value will be displayed in line 28 as an empty string.

You should run this example and access the generated web page in a web browser, and enter text, submit, etc. You can also fetch the generated page HTML using the Drakma library that we saw in the last section. Here is a code snippet using the Drakma client library to access this last example:

~~~~~~~~~
* (drakma:http-request "http://127.0.0.1:3000/hello?name=Mark")

"Hello Mark"
200
((:CONTENT-LENGTH . "10")
 (:DATE . "Fri, 05 Jul 2013 15:57:22 GMT")
 (:SERVER . "Hunchentoot 1.2.18")
 (:CONNECTION . "Close")
 (:CONTENT-TYPE . "text/plain; charset=utf-8"))
#<PURI:URI http://127.0.0.1:3000/hello?name=Mark>
#<FLEXI-STREAMS:FLEXI-IO-STREAM {10095654A3}>
T
"OK"
~~~~~~~~~

We will use both Drackma and Hunchentoot in the next section.

## Complete REST Client Server Example Using JSON for Data Serialization

A reasonable way to build modern distributed systems is to write REST web services that serve JSON data to client applications. These client applications might be rich web apps written in Javascript, other web services, and applications running on smartphones that fetch and save data to a remote web service.

We will use the **cl-json** Quicklisp package to encode Lisp data into a string representing JSON encoded data. Here is a quick example:

~~~~~~~~~
* (ql:quickload :cl-json)
* (defvar y (list (list '(cat . "the cat ran") '(dog . 101)) 1 2 3 4 5))

Y
* y

(((CAT . "the cat ran") (DOG . 101)) 1 2 3 4 5)
* (json:encode-json-to-string y)
"[{\"cat\":\"the cat ran\",\"dog\":101},1,2,3,4,5]"
~~~~~~~~~

The following list shows the contents of the file **src/web-hunchentoot-json.lisp**:

{lang="lisp",linenos=on}
~~~~~~~~~
(ql:quickload :hunchentoot)
(ql:quickload :cl-json)

(defvar *h* (make-instance 'hunchentoot:easy-acceptor :port 3000))

;; define a handler with the name animal:

(hunchentoot:define-easy-handler (animal :uri "/animal") (name)
  (print name)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((string-equal name "cat")
     (json:encode-json-to-string
       (list
        (list
         '(average_weight . 10)
         '(friendly . nil))
        "A cat can live indoors or outdoors.")))
    ((string-equal name "dog")
     (json:encode-json-to-string
       (list
        (list
         '(average_weight . 40)
         '(friendly . t))
        "A dog is a loyal creature, much valued by humans.")))
    (t
     (json:encode-json-to-string
       (list
        ()
        "unknown type of animal")))))

(hunchentoot:start *h*)
~~~~~~~~~

This example is very similar to the web application example in the last section. The difference is that this application is not intended to be viewed on a web page because it returns JSON data as HTTP responses. The easy handler definition on line 8 specifies a handler argument **name**. In lines 12 and 19 we check to see if the value of the argument **name** is "cat" or "dog" and if it is, we return the appropriate JSON example data for those animals. If there is no match, the default **cond** clause starting on line 26 returns a warning string as a JSON encoded string.

While running this test service, in one repl, you can ue the Drakma library in another repl to test it (not all output is shown in the next listing):

~~~~~~~~~
* (ql:quickload :drakma)
* (drakma:http-request "http://127.0.0.1:3000/animal?name=dog")

"[{\"average_weight\":40,
   \"friendly\":true},
   \"A dog is a loyal creature, much valued by humans.\"]"
200
* (drakma:http-request "http://127.0.0.1:3000/animal?name=cat")

"[{\"average_weight\":10,
   \"friendly\":null},
   \"A cat can live indoors or outdoors.\"]"
200
~~~~~~~~~

You can use the **cl-json** library to decode a string containing JSON data to Lisp data:

~~~~~~~~~
* (ql:quickload :cl-json)
To load "cl-json":
  Load 1 ASDF system:
    cl-json
; Loading "cl-json"
.
(:CL-JSON)
* (cl-json:decode-json-from-string
    (drakma:http-request "http://127.0.0.1:3000/animal?name=dog"))

(((:AVERAGE--WEIGHT . 40) (:FRIENDLY . T))
 "A dog is a loyal creature, much valued by humans.")
~~~~~~~~~


For most of my work, REST web services are "read-only" in the sense that clients don't modify state on the server. However, there are use cases where a client application might want to; for example, letting clients add new animals to the last example.


{lang="lisp",linenos=on}
~~~~~~~~~
(defparameter *animal-hash* (make-hash-table))

;; handle HTTP POST requests:
(hunchentoot:define-easy-handler (some-handler :uri "/add") (json-data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let* ((data-string (hunchentoot:raw-post-data :force-text t))
         (data (cl-json:decode-json-from-string json-data))
         ;; assume that the name of the animal is a hashed value:
         (animal-name (gethash "name" data)))
     (setf (gethash animal-name *animal-hash*) data))
  "OK")
~~~~~~~~~

In line 4 we are defining an additional easy handler with a handler argument **json-data**. This data is assumed to be a string encoding of JSON data which is decoded into Lisp data in lines 6 and 7. We save the data to the global variable *animal-hash*.

In this example, we are storing data sent from a client in an in-memory hash table. In a real application new data might be stored in a database.

## Network Programming Wrap Up

You have learned the basics for writing web services and writing clients to use web services. Later, we will use web services written in Python by writing Common Lisp clients: we will wrap retrained deep learning models and access them from Common Lisp.