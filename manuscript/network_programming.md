# Network Programming  {#network_prog}

Distributed computing is pervasive: you need to look no further than the World Wide Web, Internet chat, etc. Of course, as a Lisp programmer, you will want to do at least some of your network programming in Lisp! Previous editions of this book provided low level socket examples. For this new edition, I removed those and instead encourage you to work at a higher level of abstraction.

**Note: there is no `./src` example directory for this chapter. Drakma and Hunchentoot will be used later in this book. Here we just show a few interactive examples.**

We will use Edi Weitz's [Drakma HTTP client library](http://weitz.de/drakma/) for fetching web data and his [Hunchentoot web server](http://weitz.de/hunchentoot/) for building REST services that return JSON. JSON is easier to read than XML and works well with Javascript and data stores like Postgres, MongoDB, and CouchDB.


## An introduction to Drakma

Edi Weitz's [Drakma library](http://weitz.de/drakma/) supports HTTP GET, PUT, authentication, and cookies. The top level API is **drakma:http-request**, which returns multiple values. The first three values are the response body, HTTP status code, and headers:

~~~~~~~~
* (ql:quickload :drakma)
* (multiple-value-setq (data http-response-code headers)
    (drakma:http-request "http://example.com"))

* data
"<!doctype html>..."
* http-response-code
200
* headers
((:SERVER . "ECS")
 (:CONTENT-TYPE . "text/html; charset=UTF-8"))
~~~~~~~~

We will use Drakma later in this book. Next we will write a web app using Hunchentoot and test it with a Drakma client.


## An introduction to Hunchentoot

Edi Weitz's [Hunchentoot project](http://weitz.de/hunchentoot/) is a flexible library for writing web applications and services. We will also use his CL-WHO library for generating HTML from Lisp code.

{lang="lisp",linenos=on}
~~~~~~~~
(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(in-package :cl-user)
(defpackage hdemo
  (:use :cl :cl-who :hunchentoot))
(in-package :hdemo)

(defvar *h* (make-instance 'easy-acceptor :port 3000))

(define-easy-handler (my-greetings :uri "/hello") (name)
  (setf (hunchentoot:content-type*) "text/html")
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "hunchentoot test"))
     (:body
      (:h1 "hunchentoot form demo")
      (:form :method :post
       (:input :type :text :name "name" :value name)
       (:input :type :submit :value "Submit your name"))
      (:p "Hello " (str name))))))

(hunchentoot:start *h*)
~~~~~~~~

**define-easy-handler** creates an HTTP handler and adds it to the acceptor. The **:uri** keyword routes requests to this handler. The **name** parameter captures query or form data. CL-WHO generates HTML from Lisp code.

Fetch the page with Drakma to test:

~~~~~~~~
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
~~~~~~~~

## Complete REST Client Server Example Using JSON for Data Serialization

A reasonable way to build distributed systems is to write REST web services that serve JSON to clients. Clients might be rich web apps, other services, or smartphone applications.

We use **cl-json** to encode Lisp data as JSON:

~~~~~~~~
* (ql:quickload :cl-json)
* (defvar y (list (list '(cat . "the cat ran") '(dog . 101)) 1 2 3 4 5))
* (json:encode-json-to-string y)
"[{\"cat\":\"the cat ran\",\"dog\":101},1,2,3,4,5]"
~~~~~~~~

The following is the file **src/web-hunchentoot-json.lisp**:

{lang="lisp",linenos=on}
~~~~~~~~
(ql:quickload :hunchentoot)
(ql:quickload :cl-json)

(defvar *h* (make-instance 'hunchentoot:easy-acceptor :port 3000))

(hunchentoot:define-easy-handler (animal :uri "/animal") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((string-equal name "cat")
     (json:encode-json-to-string
       (list (list '(average_weight . 10) '(friendly . nil))
             "A cat can live indoors or outdoors.")))
    ((string-equal name "dog")
     (json:encode-json-to-string
       (list (list '(average_weight . 40) '(friendly . t))
             "A dog is a loyal creature, much valued by humans.")))
    (t
     (json:encode-json-to-string
       (list () "unknown type of animal")))))

(hunchentoot:start *h*)
~~~~~~~~

This handler returns JSON instead of HTML. Test it with Drakma:

~~~~~~~~
* (drakma:http-request "http://127.0.0.1:3000/animal?name=dog")

"[{\"average_weight\":40,\"friendly\":true},
  \"A dog is a loyal creature, much valued by humans.\"]"
200
* (drakma:http-request "http://127.0.0.1:3000/animal?name=cat")

"[{\"average_weight\":10,\"friendly\":null},
  \"A cat can live indoors or outdoors.\"]"
200
~~~~~~~~

Decode JSON back to Lisp data:

~~~~~~~~
* (cl-json:decode-json-from-string
    (drakma:http-request "http://127.0.0.1:3000/animal?name=dog"))

(((:AVERAGE--WEIGHT . 40) (:FRIENDLY . T))
 "A dog is a loyal creature, much valued by humans.")
~~~~~~~~

For state changes, use POST handlers:

{lang="lisp",linenos=on}
~~~~~~~~
(defparameter *animal-hash* (make-hash-table))

(hunchentoot:define-easy-handler (some-handler :uri "/add") (json-data)
  (setf (hunchentoot:content-type*) "text/plain")
  (let* ((data (cl-json:decode-json-from-string json-data))
         (animal-name (gethash "name" data)))
    (setf (gethash animal-name *animal-hash*) data))
  "OK")
~~~~~~~~

## Network Programming Wrap Up

You have learned the basics for writing web services and clients. Later, we will wrap retrained deep learning models in Python web services and access them from Common Lisp.