# Client Library for the Tavily Web Search APIs

Tavily Search APIs offer developers a simple way to integrate powerful, real-time search capabilities into their applications. By abstracting away the complexities of web crawling, indexing, and ranking, Tavily allows developers to quickly access high-quality, relevant search results from across the web with minimal coding effort. I also use commercial Search APIs from Google, Microsoft, and Brave. For my work I find APIs from Tavily and Brave are the simplest to use when prototyping.

The source code for this Tavily client library is in my GitHub repository [https://github.com/mark-watson/tavily](https://github.com/mark-watson/tavily). As usual you want to git clone this repository in your local directory **~/quicklisp/local-projects/** so Quicklisp can find this library with **(ql:quickload :tavily)**. We will list the code below and then look at example use.


### package.lisp

```lisp
;;;; package.lisp

(defpackage #:tavily
  (:use :cl)
  (:import-from :babel :octets-to-string)
  (:export #:websearch))
```

### tavily.asd

```lisp
;;;; openai.asd

(asdf:defsystem #:tavily
  :description "Library for using the perplexity search+LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:dexador :jonathan)
  :components ((:file "package")
               (:file "tavily")))
```

### tavily.lisp

The following code defines a package **:tavily** that interacts with the Tavily Search API. It declares two global variables: ***tavily-api-key***, which retrieves the API key from the environment variable TAVILY_API_KEY, and ***tavily-api-url***, set to the API’s base URL. The **parse-json** function utilizes the **cl-json** library to convert JSON strings into Lisp data structures, ensuring lists are returned as property lists. The **convert-bytes-to-string** function transforms byte arrays into UTF-8 encoded strings using the **babel** libray’s **octets-to-string** function. To construct the JSON payload for API requests, the function **make-tavily-json-payload** creates a JSON string containing the API key, search query, and a maximum result limit of five, employing **jonathan:to-json** for encoding. The **filter-tavily-response-item** function extracts specific elements from each API response item, returning a list of data. The primary function, **websearch**, accepts a search query and an optional API key. It sends a POST request to the Tavily API using **dex:post** from the **dexador** library. Upon receiving a response, it parses the JSON, checks for errors, and processes the results by mapping **filter-tavily-response-item** over the response data. Finally, it concatenates the content fields of the filtered results into a single string, separated by newlines, and returns this string.

```lisp
(in-package :tavily)

(defvar *tavily-api-key* (uiop:getenv "TAVILY_API_KEY")
  "Your Tavily Search API key.")

(defvar *tavily-api-url* "https://api.tavily.com/search"
  "Base URL for Tavily Search API.")

(defun parse-json (json-string)
  "Parses a JSON string into a Lisp data structure."
  (json:set-decoder-simple-list-semantics) ;; required for returning plist
  (cl-json:decode-json-from-string json-string))

;; Define the conversion function using the imported babel function directly
(defun convert-bytes-to-string (bytes)
  (octets-to-string bytes :encoding :utf-8))

(defun make-tavily-json-payload (query)
  "Helper function to create JSON payload for Tavily API request."
  (jonathan:to-json 
   (list :|api_key| (or *tavily-api-key* (uiop:getenv "TAVILY_API_KEY"))
         :|query| query
         :|max_results| 5)))

(defun filter-tavily-response-item (result)
  "Helper function to filter Tavily API response item."
  ;;(format t "** result: ~A~%" result)
  (list (cdr (nth 0 result))
        (cdr (nth 1 result))
        (cdr (nth 2 result))))

(defun websearch (query &key (api-key *tavily-api-key*))
  "Performs a search using the Tavily Search API."
  (format t "~%* Calling tavily-search with query: ~A~%" query)
  (let* ((api-key-to-use (or api-key (uiop:getenv "TAVILY_API_KEY")))
         (api-url *tavily-api-url*)
         (prompt-data (make-tavily-json-payload query)))

    (unless api-key-to-use
      (error "Tavily API key is not set."))

    (handler-case
      (let ((response-str
             (dex:post api-url
                       :headers '(("Content-Type" . "application/json"))
                        :content prompt-data)))
           (let ((response-json (parse-json response-str)))
             (if (getf response-json :error)
                (error "Tavily API Error: ~A" (getf response-json :error))
              ;; Process and return search results
              (let ((uri-title-content-list
                     (mapcar #'filter-tavily-response-item
                             (cdr (assoc :RESULTS response-json)))))
                ;; let's just return concatenated content
                (format nil "~{~a~%~}" (mapcar #'caddr uri-title-content-list))))))
      (error (c)
        (error "Error communicating with Tavily API: ~A" c)))))

;; (tavily:websearch "Fun things to do in Sedona Arizona")
```


## Example Use

The following listing shows the use of the Tavily client code.

```text
* (ql:quickload :tavily)
To load "tavily":
  Load 1 ASDF system:
    tavily
; Loading "tavily"
......................
(:tavily)
* (tavily:websearch "Fun things to do in Flagstaff Arizona")

* Calling tavily-search with query: Fun things to do in Flagstaff Arizona
"Top things to do in Flagstaff include Walnut Canyon & Wupatki National Monuments, nature tours, and an astronomy hub with stargazing. Cliff dwellings and ancient pueblos are also popular.
Flagstaff offers visitors seemingly endless things to do. Whether it be outdoor adventures, cultural experiences, or simply a tranquil escape from the every
1. Downtown Flagstaff. Dusk on a busy downtown street in a small town. · 2. Flagstaff Brewery Trail · 3. Lowell Observatory · 4. Museum of Northern Arizona · 5.
Explore the best things to do in Flagstaff, AZ, with our comprehensive guide! From scenic drives around the San Francisco Peaks to the
1. See what's going on at Heritage Square or Wheeler Park. · 2. Enjoy the sunshine and great weather. · 3. Visit the historic Weatherford Hotel, Hotel Monte Vista
"
```

