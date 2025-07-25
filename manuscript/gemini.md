# Client Library for the Google Gemini LLM APIs

While the Google Gemini APIs are not quite as good for coding tasks as Anthropic's Claude and OpenAI's o1-mini-high models, Gemini offers a compelling suite of advantages for developers seeking to integrate cutting-edge, multimodal AI capabilities into their applications. A primary benefit is the large one million token context size and very fast inference speeds. Gemini is very cost effective for natural language processing tasks such as text summarization, question answering, code generation, creative content creation, and conversational AI.

The source code for this Gemini library is in my GitHub repository [https://github.com/mark-watson/gemini](https://github.com/mark-watson/gemini). As usual you want to git clone this repository in your local directory **~/quicklisp/local-projects/** so Quicklisp can find this library with **(ql:quickload :gemini)**. We will list the code below and then look at example use.

### package.lisp

We need the function **post** in the external library **dexador**:

```lisp
;;;; package.lisp

(defpackage #:gemini
  (:use #:cl)
  (:import-from #:dexador
                #:post)  ; Only import the symbols we need
  (:export #:generate))
```

### gemini.asd

```lisp
;;;; gemini.asd

(asdf:defsystem #:gemini
  :description "Library for using the perplexity search+LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:dexador)
  :components ((:file "package")
               (:file "gemini")))
```

### gemini.lisp

This code defines a function that sends a user-provided text prompt to an external API for generative language processing. It first retrieves a Google API key from the environment and sets the API endpoint URL, then constructs a nested JSON payload embedding the prompt within a specific structure. Using a POST request with appropriate headers - including the API key - the function submits the payload to the API. It then decodes the JSON response, traverses the nested data to extract the generated text, and finally returns plain text as the result.

```lisp
(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *gemini-api-url*
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent")

(defun generate (prompt)
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((data (cl-json:encode-json-to-string payload))
           (response (dex:post *gemini-api-url*
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string
             (if (stringp response)
                response
                (flex:octets-to-string
                  response :external-format :utf-8)))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates-pair (assoc :CANDIDATES decoded-response))
           (candidates (cdr candidates-pair))
           (candidate (first candidates))
           (content-pair (assoc :CONTENT candidate))
           (content (cdr content-pair))
           (parts-pair (assoc :PARTS content))
           (parts (cdr parts-pair))
           (first-part (first parts))
           (text-pair (assoc :TEXT first-part))
           (text (cdr text-pair)))
       text)))

;; (gemini:generate "In one sentence, explain how AI works to a child.")
;; (gemini:generate "Write a short, four-line poem about coding in Python.")
```

## Example Use

```text
CL-USER 2 > (gemini:generate "In one sentence, explain how AI works to a child.")
"AI is like a computer that learns from lots of examples to get really good at doing things, like recognizing pictures or playing games!
"
```




