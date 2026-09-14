# One Interface for Brave, Tavily, and Perplexity Web Search APIs

*Note: this chapter replaces the separate Brave, Tavily, and Perplexity chapters from earlier editions. The three client libraries are now a single library, **search-apis**, in the directory **loving-common-lisp/src/search_APIs**.*

A web search API lets an application send a query and receive web results without crawling or indexing pages itself. Several companies offer one. Brave, Tavily, and Perplexity are the three used in this book. Each works differently:

- **Brave** takes a GET request with the subscription key in a header and returns ranked links with snippets.
- **Tavily** takes a POST request with the key in the JSON body and returns ranked links with snippets and a relevance score.
- **Perplexity** runs a search and then asks a language model to answer the question from the pages it found, returning an answer with citations.

Writing one function per API spreads provider details across the whole application. This chapter builds one library that hides those details. Every provider is called through a single function, **websearch**, and every provider returns the same **search-response** structure. Adding a provider means registering one function, not editing the call sites.

## Two Kinds of Search API

It helps to separate the providers into two groups before looking at the code.

The first group performs pure search. Brave and Tavily belong here. You send a query and get back a list of pages, each with a title, a URL, and a short text snippet. The snippet comes from the page or from the search engine's summary of it.

The second group performs search plus language model processing. Perplexity belongs here. You send a query, Perplexity searches, and a language model writes an answer that cites the pages it used. You get both an answer and a list of sources.

The library models both groups with one structure. The **results** slot always holds the list of pages. The **answer** slot holds the synthesized answer when the provider produces one, and is **nil** for pure search providers. Callers that only want links read **results** and ignore **answer**.

## The Shared Data Model

The whole point of the library is that all three providers produce the same two structures:

```lisp
(defstruct search-result
  title
  url
  content
  (score nil))

(defstruct search-response
  provider
  query
  answer
  results
  raw)
```

A **search-result** is one page. **content** holds the provider's snippet and may be **nil** when a provider returns links only. **score** holds a provider-specific relevance value and is **nil** when the provider does not supply one.

A **search-response** is the result of one call. **provider** records which API answered, **query** records the question, **answer** holds the synthesized answer or **nil**, **results** holds the list of **search-result** values, and **raw** holds the full decoded JSON in case you need a field the structures do not expose.

To see where these fields come from, here is a shortened Brave response:

```json
{
  "web": {
    "results": [
      {
        "title": "Visit Sedona | The official site of the Sedona Tourism Bureau",
        "url": "https://visitsedona.com/",
        "description": "The official site of the Sedona, AZ tourism bureau."
      }
    ]
  }
}
```

Here is a shortened Tavily response. It carries the same title, url, and content fields, plus a score:

```json
{
  "query": "Sedona Arizona",
  "results": [
    {
      "title": "Visit Sedona",
      "url": "https://visitsedona.com/",
      "content": "The official site of the Sedona, AZ tourism bureau.",
      "score": 0.98
    }
  ]
}
```

Here is a shortened Perplexity response. The answer sits under **choices**, and the sources sit under **citations** and **search_results**:

```json
{
  "choices": [
    { "message": { "content": "Sedona is in northern Arizona." } }
  ],
  "citations": ["https://visitsedona.com/", "https://www.sedonaaz.gov/"]
}
```

Each provider parses a different shape, and each parser writes the same **search-response**.

## Source Code

The library has seven files. The ASDF system lists them in load order:

```lisp
;;; search-apis.asd

(asdf:defsystem #:search-apis
  :description "One common web search interface across providers (Brave, Tavily, Perplexity)."
  :author "Mark Watson"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:dexador #:quri #:uiop)
  :components ((:file "package")
               (:file "json")
               (:file "search-apis")
               (:file "providers")))
```

The dependency list is short. **dexador** makes the HTTP requests, **quri** URL-encodes the query string, and **uiop** reads environment variables. Everything else, including the JSON codec, lives in the library.

### package.lisp

The package exports the entry point, the accessors for the two structures, the provider registry, and the condition hierarchy:

```lisp
;;; package.lisp

(defpackage #:search-apis
  (:nicknames #:search_apis)
  (:use #:cl)
  (:export
   #:websearch
   #:search-result
   #:search-result-title
   #:search-result-url
   #:search-result-content
   #:search-result-score
   #:search-response
   #:search-response-provider
   #:search-response-query
   #:search-response-answer
   #:search-response-results
   #:search-response-raw
   #:search-provider
   #:search-provider-name
   #:search-provider-base-url
   #:search-provider-env-keys
   #:define-search-provider
   #:find-search-provider
   #:search-error
   #:api-error
   #:api-error-status
   #:api-error-body
   #:authentication-error
   #:rate-limit-error
   #:not-found-error
   #:json-encode
   #:json-decode))
```

The nickname **search_apis** matches the directory name, so both **search-apis:websearch** and **search_apis:websearch** work.

### json.lisp

Every provider speaks JSON, so the library carries its own codec instead of depending on a JSON library. It encodes Lisp alists, lists, strings, numbers, and the symbols **t**, **:false**, and **:null**, and it decodes JSON back into alists with string keys. Keys are converted between Lisp and JSON conventions: a keyword such as **:max-results** becomes the JSON key **max_results**.

```lisp
;;; json.lisp

(defun json-key (key)
  "Convert a string or keyword to a JSON object key (downcased, - -> _)."
  (etypecase key
    (string key)
    (symbol (substitute #\_ #\- (string-downcase (symbol-name key))))))

(defun %write-json-string (s out)
  (write-char #\" out)
  (loop for ch across s do
    (case ch
      (#\" (write-string "\\\"" out))
      (#\\ (write-string "\\\\" out))
      (#\Newline (write-string "\\n" out))
      (#\Return (write-string "\\r" out))
      (#\Tab (write-string "\\t" out))
      (t (if (< (char-code ch) 32)
             (format out "\\u~4,'0x" (char-code ch))
             (write-char ch out)))))
  (write-char #\" out))

(defun %write-json-float (f out)
  (if (and (<= -1.0d15 f 1.0d15) (= f (truncate f)))
      (format out "~D.0" (truncate f))
      (let ((s (string-right-trim " " (format nil "~G" f))))
        (write-string s out)
        (when (char= (char s (1- (length s))) #\.)
          (write-char #\0 out)))))

(defun %json-alist-p (x)
  (and (consp x)
       (every (lambda (e) (and (consp e) (or (stringp (car e)) (symbolp (car e)))))
              x)))

(defun write-json (x out)
  (cond
    ((stringp x) (%write-json-string x out))
    ((eq x t) (write-string "true" out))
    ((eq x :false) (write-string "false" out))
    ((eq x :null) (write-string "null" out))
    ((integerp x) (princ x out))
    ((floatp x) (%write-json-float x out))
    ((realp x) (%write-json-float (coerce x 'double-float) out))
    ((%json-alist-p x)
     (write-char #\{ out)
     (loop for (k . v) in x for first = t then nil do
       (unless first (write-char #\, out))
       (%write-json-string (json-key k) out)
       (write-char #\: out)
       (write-json v out))
     (write-char #\} out))
    ((listp x)
     (write-char #\[ out)
     (loop for e in x for first = t then nil do
       (unless first (write-char #\, out))
       (write-json e out))
     (write-char #\] out))
    ((symbolp x) (%write-json-string (string-downcase (symbol-name x)) out))
    (t (error "Cannot JSON-encode ~S" x))))

(defun json-encode (x)
  "Encode the nested list structure X as a JSON string."
  (with-output-to-string (out) (write-json x out)))

(defun json-decode (string)
  "Decode a JSON string into nested alists (string keys), lists, strings,
numbers, t (true) and nil (false/null)."
  (let ((pos 0) (len (length string)))
    (labels ((peek () (and (< pos len) (char string pos)))
             (advance () (incf pos))
             (skip-ws ()
               (loop while (and (peek) (member (peek) '(#\Space #\Tab #\Newline #\Return)))
                     do (advance)))
             (expect (ch)
               (unless (eql (peek) ch)
                 (error "JSON parse error at ~D: expected ~S in ~S" pos ch string))
               (advance))
             (parse-string ()
               (expect #\")
               (with-output-to-string (out)
                 (loop for ch = (peek) do
                   (cond ((null ch) (error "Unterminated JSON string"))
                         ((char= ch #\") (advance) (return))
                         ((char= ch #\\)
                          (advance)
                          (let ((esc (peek)))
                            (advance)
                            (case esc
                              (#\n (write-char #\Newline out))
                              (#\t (write-char #\Tab out))
                              (#\r (write-char #\Return out))
                              (#\b (write-char #\Backspace out))
                              (#\f (write-char #\Page out))
                              (#\u (write-char (code-char (parse-integer string
                                                                          :start pos
                                                                          :end (+ pos 4)
                                                                          :radix 16))
                                               out)
                                   (incf pos 4))
                              (t (write-char esc out)))))
                         (t (write-char ch out) (advance))))))
             (parse-number ()
               (let ((start pos))
                 (when (eql (peek) #\-) (advance))
                 (loop while (and (peek) (digit-char-p (peek))) do (advance))
                 (let ((is-float nil))
                   (when (eql (peek) #\.)
                     (setf is-float t) (advance)
                     (loop while (and (peek) (digit-char-p (peek))) do (advance)))
                   (when (and (peek) (member (peek) '(#\e #\E)))
                     (setf is-float t) (advance)
                     (when (and (peek) (member (peek) '(#\+ #\-))) (advance))
                     (loop while (and (peek) (digit-char-p (peek))) do (advance)))
                   (let ((token (string-trim " " (subseq string start pos))))
                     (if is-float
                         (let ((*read-default-float-format* 'double-float))
                           (read-from-string token))
                         (parse-integer token))))))
             (parse-array ()
               (expect #\[) (skip-ws)
               (if (eql (peek) #\])
                   (progn (advance) nil)
                   (loop collect (parse-value) into items
                         do (skip-ws)
                            (cond ((eql (peek) #\,) (advance) (skip-ws))
                                  ((eql (peek) #\]) (advance) (return items))
                                  (t (error "JSON array parse error at ~D" pos))))))
             (parse-object ()
               (expect #\{) (skip-ws)
               (if (eql (peek) #\})
                   (progn (advance) nil)
                   (loop collect (let ((k (parse-string)))
                                   (skip-ws) (expect #\:) (skip-ws)
                                   (cons k (parse-value)))
                         into pairs
                         do (skip-ws)
                            (cond ((eql (peek) #\,) (advance) (skip-ws))
                                  ((eql (peek) #\}) (advance) (return pairs))
                                  (t (error "JSON object parse error at ~D" pos))))))
             (parse-literal (word value)
               (unless (and (<= (+ pos (length word)) len)
                            (string= word string :start2 pos :end2 (+ pos (length word))))
                 (error "JSON literal parse error at ~D" pos))
               (incf pos (length word))
               value)
             (parse-value ()
               (skip-ws)
               (let ((ch (peek)))
                 (cond
                   ((null ch) (error "Unexpected end of JSON input"))
                   ((char= ch #\") (parse-string))
                   ((char= ch #\{) (parse-object))
                   ((char= ch #\[) (parse-array))
                   ((char= ch #\t) (parse-literal "true" t))
                   ((char= ch #\f) (parse-literal "false" nil))
                   ((char= ch #\n) (parse-literal "null" nil))
                   ((or (digit-char-p ch) (char= ch #\-)) (parse-number))
                   (t (error "JSON parse error at ~D: ~S" pos ch))))))
      (parse-value))))

(defun aget (alist key)
  "Fetch KEY (string) from a decoded JSON alist."
  (cdr (assoc key alist :test #'equal)))
```

Two details matter for the provider code. The encoder writes **:false** as **false** and **:null** as **null**, which lets a provider send a JSON boolean or a JSON null. The decoder returns **t** for **true** and **nil** for both **false** and **null**, so a provider that needs to tell them apart reads the **raw** slot. The **aget** helper looks up a string key in a decoded object.

### search-apis.lisp

This file holds the shared core: the conditions, the two structures, the provider registry, the HTTP helpers, and the **websearch** entry point.

```lisp
;;; search-apis.lisp

(in-package #:search-apis)

;;; ---- conditions ----

(define-condition search-error (simple-error) ())

(define-condition api-error (search-error)
  ((status :initarg :status :reader api-error-status)
   (body :initarg :body :reader api-error-body))
  (:report (lambda (c stream)
             (format stream "Web search API error ~A: ~A"
                     (api-error-status c) (api-error-body c)))))

(define-condition authentication-error (api-error) ())
(define-condition rate-limit-error (api-error) ())
(define-condition not-found-error (api-error) ())

(defun %map-http-error (status body)
  "Map an HTTP status code to the corresponding search-apis condition."
  (let ((condition
          (cond ((member status '(401 403)) 'authentication-error)
                ((= status 429) 'rate-limit-error)
                ((= status 404) 'not-found-error)
                (t 'api-error))))
    (error condition :status status :body body)))

;;; ---- result and response ----

(defstruct search-result
  "One web search hit. CONTENT is the provider's snippet/summary and may be
nil when the provider only returns links. SCORE is provider specific."
  title
  url
  content
  (score nil))

(defstruct search-response
  "The result of a WEBSEARCH call. ANSWER is populated by search-plus-LLM
providers such as Perplexity; RESULTS is the list of SEARCH-RESULTs."
  provider
  query
  answer
  results
  raw)

;;; ---- provider registry ----

(defstruct search-provider
  name
  base-url
  env-keys
  (requires-key t)
  function)

(defvar *search-providers* (make-hash-table :test 'eq))

(defun define-search-provider (name base-url &key env-keys (requires-key t) function)
  "Register a search provider."
  (setf (gethash name *search-providers*)
        (make-search-provider
         :name name
         :base-url base-url
         :env-keys (if (listp env-keys) env-keys (list env-keys))
         :requires-key requires-key
         :function function)))

(defun find-search-provider (name)
  (or (gethash name *search-providers*)
      (error 'search-error
             :format-control "Unknown search provider ~S. Known providers: ~S"
             :format-arguments
             (list name (loop for k being the hash-keys of *search-providers*
                              collect k)))))

(defun provider-api-key (provider explicit-key)
  (or explicit-key
      (loop for var in (search-provider-env-keys provider)
            for value = (uiop:getenv var)
            when (and value (plusp (length value))) return value)
      (if (search-provider-requires-key provider)
          (error 'search-error
                 :format-control "No API key for provider ~S. Pass :api-key or set one of ~S"
                 :format-arguments (list (search-provider-name provider)
                                         (search-provider-env-keys provider)))
          nil)))

;;; ---- HTTP ----

(defun %get-json (url headers)
  "GET URL and decode the JSON body into a Lisp alist."
  (handler-case
      (json-decode (dex:get url :headers headers))
    (dex:http-request-failed (e)
      (%map-http-error (dex:response-status e) (dex:response-body e)))))

(defun %post-json (url headers payload)
  "POST PAYLOAD (a nested alist) as JSON to URL and decode the response."
  (handler-case
      (json-decode (dex:post url :headers headers :content (json-encode payload)))
    (dex:http-request-failed (e)
      (%map-http-error (dex:response-status e) (dex:response-body e)))))

;;; ---- main entry point ----

(defun websearch (query &key (provider :brave) api-key max-results model)
  "Search the web for QUERY using PROVIDER, a keyword such as :brave, :tavily
or :perplexity. MAX-RESULTS is a hint (used by Brave and Tavily). MODEL selects
the search-plus-LLM model for Perplexity (default \"sonar-pro\")."
  (let* ((p (find-search-provider provider))
         (key (provider-api-key p api-key)))
    (funcall (search-provider-function p) p query
             :api-key key
             :max-results (or max-results 5)
             :model model)))
```

The provider registry follows the same pattern as the **litelm** library in **loving-common-lisp/src/litelm**. A **search-provider** records the provider's name, its endpoint, the environment variables that may hold its key, and the function that performs the search. **define-search-provider** stores one in the **\*search-providers\*** hash table. **find-search-provider** looks one up by keyword and signals a **search-error** for an unknown name.

**provider-api-key** resolves the key in the order a caller expects: an explicit **:api-key** first, then the environment variables in order, and finally an error when the provider needs a key and none is found.

The two HTTP helpers wrap **dex:get** and **dex:post**. Both catch Dexador's **http-request-failed** condition and pass the status code to **%map-http-error**, which turns 401 and 403 into **authentication-error**, 429 into **rate-limit-error**, 404 into **not-found-error**, and everything else into **api-error**. The status and body travel with the condition, so a caller can log them.

**websearch** ties it together. It finds the provider, resolves the key, and calls the provider function with the query and the four options. Providers ignore the options that do not apply to them.

### providers.lisp

Each provider has two functions. A **parse** function turns a decoded JSON response into a **search-response**. A **search** function builds the request, sends it, and calls the parser. Keeping the parsers separate means they can be tested offline against recorded JSON.

The following diagram shows the high-level architecture of the Brave client:

{width: "80%"}
![Brave search architecture](images/brave_search_architecture.png)

Brave uses a GET request and puts the key in the **X-Subscription-Token** header. The query is URL-encoded, so spaces and special characters travel safely in the query string:

```lisp
;;; ---- Brave: GET, key in the X-Subscription-Token header ----

(defun parse-brave-response (json query)
  "Turn a decoded Brave web search response into a SEARCH-RESPONSE."
  (let* ((web (aget json "web"))
         (items (and web (aget web "results"))))
    (make-search-response
     :provider :brave
     :query query
     :results (loop for item in items
                    collect (make-search-result
                             :title (aget item "title")
                             :url (aget item "url")
                             :content (aget item "description")))
     :raw json)))

(defun brave-search (provider query &key api-key (max-results 5) model &allow-other-keys)
  (declare (ignore model))
  (let* ((url (format nil "~A?q=~A&count=~D"
                      (search-provider-base-url provider)
                      (quri:url-encode query :space-to-plus t)
                      max-results))
         (headers (list (cons "Accept" "application/json")
                        (cons "X-Subscription-Token" api-key)))
         (json (%get-json url headers)))
    (parse-brave-response json query)))
```

Brave nests its hits under **web.results**, and the snippet field is named **description**. The parser reads those fields and discards the rest. Because Brave has no synthesized answer, **answer** stays **nil**.

The following diagram shows the high-level architecture of the Tavily client:

{width: "80%"}
![Tavily search architecture](images/tavily_architecture.png)

Tavily uses a POST request and puts the key in the JSON body along with the query and the result limit. The parser also checks the top-level **error** field that Tavily uses for application-level errors, which arrive with an HTTP 200 status and would otherwise pass silently:

```lisp
;;; ---- Tavily: POST, key in the JSON body ----

(defun parse-tavily-response (json query)
  "Turn a decoded Tavily search response into a SEARCH-RESPONSE."
  (when (aget json "error")
    (error 'search-error
           :format-control "Tavily API error: ~A"
           :format-arguments (list (aget json "error"))))
  (make-search-response
   :provider :tavily
   :query query
   :results (loop for item in (aget json "results")
                  collect (make-search-result
                           :title (aget item "title")
                           :url (aget item "url")
                           :content (aget item "content")
                           :score (aget item "score")))
   :raw json))

(defun tavily-search (provider query &key api-key (max-results 5) model &allow-other-keys)
  (declare (ignore model))
  (let ((headers '(("Content-Type" . "application/json")))
        (payload `(("api_key" . ,api-key)
                   ("query" . ,query)
                   ("max_results" . ,max-results))))
    (parse-tavily-response
     (%post-json (search-provider-base-url provider) headers payload)
     query)))
```

Tavily names its snippet field **content** and supplies a **score** between 0 and 1, which the parser copies into the result.

The following diagram shows the high-level architecture of the Perplexity client:

{width: "80%"}
![Perplexity architecture](images/perplexity_architecture.png)

Perplexity uses the OpenAI-compatible chat completions endpoint. The key travels in the **Authorization** header as a Bearer token, and the query becomes the single user message. The response carries the answer under **choices**, and the sources under **search_results** or **citations**:

```lisp
;;; ---- Perplexity: POST chat completion, answer plus citations ----

(defun parse-perplexity-response (json query)
  "Turn a decoded Perplexity chat response into a SEARCH-RESPONSE. The LLM
answer goes in the ANSWER slot; citations/search_results become RESULTS."
  (let* ((choice (first (aget json "choices")))
         (message (and choice (aget choice "message")))
         (search-results (aget json "search_results"))
         (citations (aget json "citations")))
    (make-search-response
     :provider :perplexity
     :query query
     :answer (and message (aget message "content"))
     :results (cond
                (search-results
                 (loop for item in search-results
                       collect (make-search-result
                                :title (aget item "title")
                                :url (aget item "url")
                                :content (aget item "snippet")
                                :score (aget item "score"))))
                (citations
                 (loop for url in citations
                       collect (make-search-result :url url)))
                (t nil))
     :raw json)))

(defun perplexity-search (provider query &key api-key model max-results &allow-other-keys)
  (declare (ignore max-results))
  (let ((headers (list (cons "Content-Type" "application/json")
                       (cons "Authorization" (concatenate 'string "Bearer " api-key))))
        (payload `(("model" . ,(or model "sonar-pro"))
                   ("messages" . ((("role" . "user")
                                   ("content" . ,query)))))))
    (parse-perplexity-response
     (%post-json (search-provider-base-url provider) headers payload)
     query)))
```

Perplexity's parser is the only one that fills the **answer** slot. It prefers the richer **search_results** array, which has titles and snippets, and falls back to the plain **citations** list of URLs when that array is absent. The **or** around the model name means the default **sonar-pro** applies when the caller does not choose a model.

The file ends by registering all three providers:

```lisp
;;; ---- registration ----

(define-search-provider :brave "https://api.search.brave.com/res/v1/web/search"
  :env-keys '("BRAVE_SEARCH_API_KEY")
  :function #'brave-search)

(define-search-provider :tavily "https://api.tavily.com/search"
  :env-keys '("TAVILY_API_KEY")
  :function #'tavily-search)

(define-search-provider :perplexity "https://api.perplexity.ai/chat/completions"
  :env-keys '("PERPLEXITY_API_KEY")
  :function #'perplexity-search)
```

## Setting the API Keys

Each provider reads its key from an environment variable. Set the ones you plan to use:

```bash
export BRAVE_SEARCH_API_KEY=BSGhQ-Nd-......
export TAVILY_API_KEY=tvly-......
export PERPLEXITY_API_KEY=pplx-......
```

Get keys from the provider sites:

- Brave: [https://brave.com/search/api/](https://brave.com/search/api/). The free tier allows 2000 queries a month.
- Tavily: [https://tavily.com/](https://tavily.com/).
- Perplexity: [https://www.perplexity.ai/](https://www.perplexity.ai/).

A caller can also pass a key with **:api-key**, which takes precedence over the environment.

## Running the Code

The library is in the book repository under **src/search_APIs**. Point ASDF at the system file and load it. With SBCL:

```bash
sbcl --no-userinit --non-interactive \
  --eval '(load "~/quicklisp/setup.lisp")' \
  --eval '(asdf:load-asd "search-apis.asd")' \
  --eval '(asdf:load-system :search-apis)'
```

For interactive use, load the system in the REPL and call **websearch**. This example asks Brave for three results and prints each one:

```lisp
* (ql:quickload :search-apis)
* (dolist (r (search-apis:search-response-results
             (search-apis:websearch "Sedona Arizona" :provider :brave :max-results 3)))
    (format t "~A~%  ~A~%  ~A~%~%"
            (search-apis:search-result-title r)
            (search-apis:search-result-url r)
            (search-apis:search-result-content r)))
Visit Sedona | The official site of the Sedona Tourism Bureau
  https://visitsedona.com/
  The official site of the Sedona, AZ tourism bureau.

City of Sedona | Home
  https://www.sedonaaz.gov/
  Official site for the City of Sedona, Arizona.

Sedona, Arizona - Wikipedia
  https://en.wikipedia.org/wiki/Sedona,_Arizona
  Sedona is a city in the northern Verde Valley region of Arizona.
```

Tavily returns the same fields, plus a score. The loop is identical except for the provider:

```lisp
* (dolist (r (search-apis:search-response-results
             (search-apis:websearch "Fun things to do in Flagstaff Arizona"
                                    :provider :tavily :max-results 3)))
    (format t "~A (~,2F)~%  ~A~%  ~A~%~%"
            (search-apis:search-result-title r)
            (search-apis:search-result-score r)
            (search-apis:search-result-url r)
            (search-apis:search-result-content r)))
Top Things to Do in Flagstaff, AZ (0.99)
  https://www.visitflagstaff.com/things-to-do/
  Explore the best things to do in Flagstaff, from scenic drives around the
  San Francisco Peaks to Lowell Observatory and the historic downtown.

Downtown Flagstaff (0.97)
  https://example.com/downtown-flagstaff
  Heritage Square, Wheeler Park, and the Weatherford Hotel anchor a walkable
  downtown.

Lowell Observatory (0.95)
  https://lowell.edu/
  An astronomy hub with stargazing and the discovery of Pluto.
```

Perplexity returns an answer in addition to its sources. Read **search-response-answer** for the text and **search-response-results** for the citations:

```lisp
* (let ((resp (search-apis:websearch "Where is Sedona Arizona?" :provider :perplexity)))
    (format t "~A~%" (search-apis:search-response-answer resp))
    (format t "Sources:~%")
    (dolist (r (search-apis:search-response-results resp))
      (format t "  ~A~%" (search-apis:search-result-url r))))
Sedona is in **northern Arizona**, in the **Verde Valley**, about 30 miles
south of Flagstaff and roughly 115 miles north of Phoenix. It sits on the
county line between Coconino and Yavapai counties, near Oak Creek Canyon.
Sources:
  https://en.wikipedia.org/wiki/Sedona,_Arizona
  https://visitsedona.com/
  https://www.sedonaaz.gov/
```

## Interpreting the Results

The three listings show the same call pattern and the same accessors, which is the goal of the library. What differs is what each provider returns and what that data is good for.

Brave and Tavily give you a list of pages. Use them when you need to present links, build your own context for a later model call, or check what the web says about a topic. The **content** field is a short snippet, not the full page. Treat it as a hint that helps a reader choose a link, or as a small amount of grounding text. If you need the full page, fetch the URL yourself, as the web scraping chapter describes.

The **score** field is provider-specific. Tavily's score is a relevance value between 0 and 1, so 0.99 means Tavily considers the page a strong match. Brave does not return a comparable score in the fields this parser reads, so its results keep **nil**. Never compare scores across providers; they are on different scales and mean different things.

Perplexity gives you an answer plus its sources. This is useful when the question is what matters and the reader does not need every link. The answer is generated text, so it can be wrong even when the sources are correct. Always keep the citations and check the answer against them when the stakes are high. The **results** list from Perplexity may be shorter than a Brave or Tavily list, because it reflects the pages the model chose to cite rather than a ranked result set.

The **raw** slot holds the full decoded JSON for each provider. When you need a field that the shared structures omit, such as Tavily's **answer** summary or Perplexity's token usage, read it from **raw**. This keeps the shared interface small while leaving the full response available.

## Adding a Provider

Any search API can join the library at runtime. Write a parse function and a search function, then register them:

```lisp
(search-apis:define-search-provider :my-engine "https://api.example.com/search"
  :env-keys '("MY_ENGINE_API_KEY")
  :function #'my-engine-search)
```

The search function is called as

```lisp
(fn provider query :api-key key :max-results n :model m &allow-other-keys)
```

and must return a **search-response**. Once registered, **websearch** routes **:my-engine** to it with no other changes.

## Error Handling

Failures map onto a condition hierarchy, so callers can react to the cause instead of parsing an HTTP status:

```lisp
(handler-case
    (search-apis:websearch "Sedona" :provider :tavily)
  (search-apis:rate-limit-error (c)
    (format t "Rate limited, status ~A. Back off and retry.~%"
            (search-apis:api-error-status c)))
  (search-apis:authentication-error (c)
    (format t "Bad API key, status ~A.~%" (search-apis:api-error-status c)))
  (search-apis:api-error (c)
    (format t "Other API error ~A: ~A~%"
            (search-apis:api-error-status c)
            (search-apis:api-error-body c))))
```

**authentication-error** covers 401 and 403 and usually means a missing or wrong key. **rate-limit-error** covers 429 and means you should slow down or wait. **not-found-error** covers 404 and usually means a wrong endpoint. Every **api-error** carries the status and body, so logging is simple.

## Testing the Library

The **tests.lisp** file checks the JSON codec, the provider registry, and the three parsers against recorded JSON. Those checks run without network access. The file also runs a live check for each provider whose key is present in the environment. Run the tests with SBCL:

```bash
sbcl --no-userinit --non-interactive \
  --eval '(load "~/quicklisp/setup.lisp")' \
  --eval '(asdf:load-asd "search-apis.asd")' \
  --eval '(asdf:load-system :search-apis)' \
  --load tests.lisp
```

The parsers are the part most likely to break when a provider changes its response shape, so the offline checks matter. Here is how the Brave parser check looks:

```lisp
(let ((resp (search-apis::parse-brave-response
            (search-apis:json-decode
             (concatenate 'string
               "{\"web\":{\"results\":["
               "{\"title\":\"Sedona\",\"url\":\"https://example.com/sedona\","
               "\"description\":\"A city in Arizona\"}]}}"))
            "Sedona")))
  (check (eq :brave (search-apis:search-response-provider resp)))
  (let ((r (first (search-apis:search-response-results resp))))
    (check (string= "Sedona" (search-apis:search-result-title r)))
    (check (string= "A city in Arizona" (search-apis:search-result-content r)))))
```

When all checks pass, the test run prints:

```text
--- live BRAVE_SEARCH_API_KEY tests ---
brave => 3 results
--- live TAVILY_API_KEY tests ---
tavily => 3 results
--- live PERPLEXITY_API_KEY tests ---
perplexity => "Sedona is in northern Arizona, in the Verde Valley..."
0 failure(s).
```

A non-zero failure count exits with a non-zero status, which makes the file usable in a build script.

## Wrap Up

The **search-apis** library turns three unrelated web search APIs into one call. Brave, Tavily, and Perplexity differ in their HTTP method, their key location, their request body, and their response shape. The library absorbs those differences behind a provider registry and two shared structures.

The design has three parts. The **search-result** and **search-response** structures define one vocabulary for search data, with **results** for links and **answer** for synthesized text. The registry maps a keyword such as **:brave** to a provider record, so **websearch** stays small and adding a provider touches one place. The condition hierarchy turns HTTP failures into named conditions, so callers handle a bad key or a rate limit without inspecting status codes.

The self-contained JSON codec keeps the dependency list short. That costs some code, but it means the library builds with **dexador**, **quri**, and **uiop** alone.

Two habits are worth keeping. First, keep the parser functions separate from the request functions, so you can test them offline and catch a provider's format change before it reaches users. Second, read the **raw** slot when you need a provider-specific field instead of growing the shared structures for one caller.

## Optional Practice Problems

1. **Add a fourth provider**: Register a provider for another search API, such as Google Custom Search or Bing, by writing a parse function and a search function and calling **define-search-provider**. The new provider must return a **search-response** and work through **websearch** without any change to the core file.

2. **Sort and filter results**: Write a function **best-results** that takes a **search-response** and returns only the results whose score is above a threshold, sorted from highest to lowest. Decide what to do when a provider returns **nil** scores, and document the choice.

3. **Unified answer function**: Write **answer-question** that tries a pure search provider, builds a prompt from the top results, and sends that prompt to a language model to produce an answer with citations. Compare its output with Perplexity's answer for the same query.

4. **Retry with backoff**: Wrap **websearch** in a function **websearch-with-retry** that catches **rate-limit-error** and retries a fixed number of times with increasing delay. Use **sleep** between attempts and give up after the last one.

5. **Result cache**: Add an in-memory cache keyed by the provider and query so a repeated search returns the stored **search-response** instead of calling the API again. Include a way to clear the cache, and explain when a cache would return stale results.
