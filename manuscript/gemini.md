# Client Library for the Google Gemini LLM APIs

While the Google Gemini APIs offer a compelling suite of advantages for developers seeking to integrate cutting-edge, multimodal AI capabilities into their applications. A primary benefit is the large one million token context size and very fast inference speeds. Gemini is very cost effective for natural language processing tasks such as text summarization, question answering, code generation, creative content creation, and conversational AI.

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

Note: later we will look at the last part of the file **gemini.lisp** for code to use Google’s “grounding search”.

```lisp
(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *gemini-api-base-url*
  "https://generativelanguage.googleapis.com/v1beta/models/")

(defun generate (model-id prompt)
  "Generates text from a given prompt using the specified model.
   MODEL-ID: The ID of the model to use.
   PROMPT: The text prompt to generate content from.
   Returns the generated text as a string."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
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
  
;; (gemini:generate "gemini-2.0-flash" "In one sentence, explain how AI works to a child.")
;; (gemini:generate "gemini-2.5-flash-preview-05-20" "Write a short, four-line poem about coding in Python.")

(defun count-tokens (model-id prompt)
  "Counts the number of tokens for a given prompt and model.
MODEL-ID: The ID of the model to use (e.g., \"gemini-1.5-pro-latest\", \"gemini-1.5-flash-latest\").
PROMPT: The text prompt to count tokens for.
Returns the total token count as an integer."
  (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":countTokens"))
         (payload (make-hash-table :test 'equal)))
    ;; Construct payload similar to generate function
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
           (decoded-response (cl-json:decode-json-from-string response-string))
           ;; cl-json by default uses :UPCASE for keys, so :TOTAL-TOKENS should be correct
           (total-tokens-pair (assoc :TOTAL-TOKENS decoded-response)))
      (if total-tokens-pair
          (cdr total-tokens-pair)
          (error "Could not retrieve token count from API response: ~S" decoded-response)))))

;; (gemini:count-tokens "gemini-2.0-flash" "In one sentence, explain how AI works to a child.")

(defun run-tests ()
  "Runs tests for generate and count-tokens functions."
  (let* ((model-id "gemini-2.0-flash")
         (prompt "In one sentence, explain how AI works to a child.")
         (generated-text (generate model-id prompt))
         (token-count (count-tokens model-id prompt)))
    (format t "Generated Text: ~A~%Token Count: ~A~%" generated-text token-count)))

;; Running the test
;; (gemini::run-tests)

(defparameter *chat-history* '())

(defun chat ()
  (let ((*chat-history* ""))
   (loop
     (princ "Enter a prompt: ")
     (finish-output)
     (let ((user-prompt (read-line)))
       (princ user-prompt)
       (finish-output)
       (let ((gemini-response (gemini:generate "gemini-2.0-flash"
                (concatenate 'string *chat-history* "\nUser: " user-prompt))))
         (princ gemini-response)
         (finish-output)
         (setf *chat-history*
               (concatenate 'string "User: " user-prompt "\n" "Gemini: " gemini-response
                                  "\n" *chat-history* "\n\n")))))))

;; (gemini::chat)
```

## Example Use

```text
CL-USER 4 > (gemini:generate "gemini-2.5-flash" "In one sentence, explain how AI works to a child.")
"AI is like teaching a computer with lots and lots of examples, so it can learn to figure things out and act smart all by itself."

CL-USER 5 > (gemini:count-tokens "gemini-2.5-flash" "How many tokens is this sentence?")
7
```

## Using Google’s “Grounding Search”

Google’s “Grounding with Google Search” is a powerful feature that connects the Gemini model to real-time web information, allowing it to answer queries about current events and reducing the likelihood of "hallucinations" by anchoring responses in verifiable external data. The following Common Lisp program utilizes this feature by defining a function, generate-with-search, which builds a JSON payload that specifically includes a tools configuration. By inserting an empty Google Search object into this configuration, the code explicitly instructs the API to perform a web search—such as looking up the winner of a recent tournament like Euro 2024—and synthesize those findings into the final response, which is then parsed and returned as a string. The following code snippet is near the bottom of the file **gemini.lisp**:

```lisp
(defun generate-with-search (model-id prompt)
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (setf (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "google_search" tool)
                        (make-hash-table :test 'equal))
                  tool)))
    (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
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

;; (gemini::generate-with-search "gemini-2.5-flash" "Who won the euro 2024?")
```

The core mechanism of this implementation relies on the payload hash table construction, specifically where the tools key is populated. Unlike a standard generation request, this payload includes a list containing a Google Search object; the presence of this specific key acts as a switch, granting the model permission to query Google's search index before formulating its answer. This is particularly critical for the example query regarding "Euro 2024," as the model's static training data would likely cut off before the event occurred, making the search tool indispensable for factual accuracy.

Once the API processes the request, the function handles the JSON response by traversing the nested structure of candidates and content parts. While the API response for a grounded query actually includes rich metadata—such as groundingMetadata with search queries, source titles, and URLs—this specific function is designed to filter that out, drilling down solely to extract the synthesized text string from the first part of the first candidate. This provides a clean, human-readable answer while abstracting away the complexity of the underlying search-and-retrieve operations that generated it.

Here is example output:

```lisp
$ sbcl
This is SBCL 2.5.10, an implementation of ANSI Common Lisp.
* (ql :gemini)
To load "gemini":
  Load 1 ASDF system:
    gemini
; Loading "gemini"
nil
* (gemini::generate-with-search "gemini-2.5-flash" "Who won the euro 2024?")
"Spain won the UEFA Euro 2024 tournament, defeating England 2-1 in the final held in Berlin. This victory marked Spain's record-breaking fourth European Championship title. They achieved this feat by winning all seven of their matches throughout the tournament."
* 
```

## Using Google’s “Grounding Search” With Citations

This example is also near the bottom of the file **gemini.lisp** and adds the display of citations that consist of web sites searched. You can use the code in the web spidering chapter to fetch the text contents of these reference search results.

Here is the added code:

```lisp
defun generate-with-search-and-citations (model-id prompt)
  (let* ((payload (make-hash-table :test 'equal)))
    ;; Payload construction same as previous example):
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (setf (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "google_search" tool)
                        (make-hash-table :test 'equal))
                  tool)))
    
    (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates-pair (assoc :CANDIDATES decoded-response))
           (candidates (cdr candidates-pair))
           (candidate (first candidates))
           ;; 1. Extract Content Text
           (content-pair (assoc :CONTENT candidate))
           (content (cdr content-pair))
           (parts-pair (assoc :PARTS content))
           (parts (cdr parts-pair))
           (first-part (first parts))
           (text-pair (assoc :TEXT first-part))
           (text (cdr text-pair))
           ;; 2. Extract Grounding Metadata
           (metadata-pair (assoc :GROUNDING-METADATA candidate))
           (metadata (cdr metadata-pair))
           (chunks-pair (assoc :GROUNDING-CHUNKS metadata))
           (chunks (cdr chunks-pair))
           ;; 3. Loop through chunks to find Web sources
           (citations (loop for chunk in chunks
                            for web-data-pair = (assoc :WEB chunk)
                            for web-data = (cdr web-data-pair)
                            when web-data
                            collect (cons (cdr (assoc :TITLE web-data))
                                          (cdr (assoc :URI web-data))))))
      ;; Return both text and citations
      (values text citations))))
```

Here is the sample output (output shortened: redirect URIs shortened for brevity):

```lisp
* (multiple-value-bind (response sources)
    (gemini::generate-with-search-and-citations "gemini-2.0-flash" "Who won the Super Bowl in 2024?")
  (format t "Answer: ~a~%~%Sources:~%" response)
  (loop for (title . url) in sources
        do (format t "- ~a: ~a~%" title url)))

Answer: The Kansas City Chiefs won Super Bowl LVIII in 2024, defeating the San Francisco 49ers 25-22 in overtime. The game was held in Las Vegas on February 11, 2024. This victory marked the Chiefs' third Super Bowl title in five years and made them the first back-to-back NFL champions in almost 20 years. Patrick Mahomes, the Chiefs' quarterback, was named Super Bowl MVP for the third time.

Sources:
- olympics.com: https://vertexaisearch.cloud.google.com/grounding-api-redirect/AUjJ4MF1eTcc...
- kcur.org: https://vertexaisearch.cloud.google.com/grounding-api-redirect/AUOKyR9rZ...
- theguardian.com: https://vertexaisearch.cloud.google.com/grounding-api-redirect/AUZItCsAVG...
- foxsports.com: https://vertexaisearch.cloud.google.com/grounding-api-redirect/AUZIYrbCcX...
nil
* 
```

