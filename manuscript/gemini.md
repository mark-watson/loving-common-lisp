# Client Library for the Google Gemini LLM APIs

While the Google Gemini APIs offer a compelling suite of advantages for developers seeking to integrate cutting-edge, multimodal AI capabilities into their applications. A primary benefit is the large one million token context size and very fast inference speeds. Gemini is very cost effective for natural language processing tasks such as text summarization, question answering, code generation, creative content creation, and conversational AI.

**Note: March 22, 2026 addition: new section on Gemini Interaction APIs to blend local tools and Google’s built in tools for search, etc. New material is at the end of this chapter.**

The source code for this Gemini library is in my GitHub repository [https://github.com/mark-watson/gemini](https://github.com/mark-watson/gemini). As usual you want to git clone this repository in your local directory **~/quicklisp/local-projects/** so Quicklisp can find this library with **(ql:quickload :gemini)**. We will list the code below and then look at example use.

### package.lisp

We need the function **post** in the external library **dexador**:

```lisp
;;;; package.lisp

(defpackage #:gemini
  (:use #:cl)

  (:export #:generate #:count-tokens #:send-chat-message
           #:generate-streaming #:generate-with-search
           #:generate-with-search-and-citations
           #:make-function-declaration #:generate-with-tools
           #:continue-with-function-responses))
```

### gemini.asd

```lisp
;;;; gemini.asd

(asdf:defsystem #:gemini
  :description "Library for using the perplexity search+LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:alexandria)
  :components (
         (:file "package")
         (:file "gemini")
	       (:file "gemini_interactions_api")))

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

(defvar *model* "gemini-3-flash-preview") ;; model used in this file.

(defun escape-json (json-string)
  (with-output-to-string (s)
    (loop for char across json-string
          do (case char
               (#\" (write-string "\\\"" s))
               (#\\ (write-string "\\\\" s))
               (t (write-char char s))))))

(defun run-curl-command (curl-command)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program curl-command
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (if (zerop exit-code)
        output
        (error "Curl command failed: ~A~%Error: ~A" curl-command error-output))))

(defun generate (prompt &optional (model-id *model*))
  "Generates text from a given prompt using the specified model.
   Uses *model* defined at the top of this file as default.
   PROMPT: The text prompt to generate content from.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the generated text as a string."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((api-url (concatenate 'string *gemini-api-base-url*
				 model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (escaped-json (escape-json data))
           (curl-cmd
	    (format
	     nil
	     "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
             api-url *google-api-key* escaped-json))
           (response-string (run-curl-command curl-cmd))
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

(defun count-tokens (prompt &optional (model-id *model*))
  "Counts the number of tokens for a given prompt and model.
   Uses *model* defined at top of this file as default.
   PROMPT: The text prompt to count tokens for.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the total token count as an integer."
  (let* ((api-url (concatenate 'string
			       *gemini-api-base-url* model-id ":countTokens"))
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
           (escaped-json (escape-json data))
           (curl-cmd
	    (format
	     nil
	     "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
             api-url *google-api-key* escaped-json))
           (response-string (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           ;; cl-json by default uses :UPCASE for keys,
	   ;; so :TOTAL-TOKENS should be correct
           (total-tokens-pair (assoc :TOTAL-TOKENS decoded-response)))
      (if total-tokens-pair
          (cdr total-tokens-pair)
          (error "Could not retrieve token count from API response: ~S"
		 decoded-response)))))

;; (gemini:count-tokens "In one sentence, explain how AI works to a child.")

(defun run-tests ()
  "Runs tests for generate and count-tokens functions."
  (let* ((prompt "In one sentence, explain how AI works to a child.")
         (generated-text (generate prompt))
         (token-count (count-tokens prompt)))
    (format t "Generated Text: ~A~%Token Count: ~A~%" generated-text token-count)))
```

## Example Use

```text
CL-USER 4 > (gemini:generate "In one sentence, explain how AI works to a child.")
"AI is like teaching a computer with lots and lots of examples, so it can learn to figure things out and act smart all by itself."

CL-USER 5 > (gemini:count-tokens "How many tokens is this sentence?")
7
```

## Using Google’s “Grounding Search”

Google’s “Grounding with Google Search” is a powerful feature that connects the Gemini model to real-time web information, allowing it to answer queries about current events and reducing the likelihood of "hallucinations" by anchoring responses in verifiable external data. The following Common Lisp program utilizes this feature by defining a function, generate-with-search, which builds a JSON payload that specifically includes a tools configuration. By inserting an empty Google Search object into this configuration, the code explicitly instructs the API to perform a web search—such as looking up the winner of a recent tournament like Euro 2024—and synthesize those findings into the final response, which is then parsed and returned as a string. The following code snippet is near the bottom of the file **gemini.lisp**:

```lisp
(defun generate-with-search (prompt &optional (model-id *model*))
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
           (escaped-json (escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string (run-curl-command curl-cmd))
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

;; (gemini:generate-with-search "Consultant Mark Watson has written Common Lisp, semantic web, Clojure, Java, and AI books. What musical instruments does he play?")
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
* (gemini:generate-with-search "What movies are playing in Flagstaff Arizona today at the Harkens Theater?")
"Today, **Thursday, December 18, 2025**, the following movies are playing at the **Harkins Flagstaff 16** theater. 

Please note that many major releases, such as *Avatar: Fire and Ash*, are currently running early \"preview\" screenings ahead of their official opening tomorrow.

### **Movies & Showtimes**

*   **Avatar: Fire and Ash** (PG-13)
    *   **Ciné XL:** 2:00 PM, 6:15 PM, 10:30 PM
    *   **3D HFR:** 2:30 PM, 5:00 PM, 6:45 PM, 9:15 PM
    *   **Digital:** 3:00 PM, 4:00 PM, 7:15 PM, 8:15 PM, 9:30 PM
*   **Five Nights at Freddy's 2** (PG-13)
    *   11:30 AM, 12:30 PM, 2:05 PM, 4:50 PM, 7:35 PM, 10:20 PM
*   **Zootopia 2** (PG)
    *   1:45 PM, 5:10 PM, 7:25 PM, 8:10 PM
*   **David** (PG)
    *   12:45 PM, 3:30 PM, 6:15 PM, 9:00 PM
*   **Ella McCay** (PG-13)
    *   10:20 AM, 1:05 PM, 3:50 PM, 6:35 PM
*   **Eternity** (PG-13)
    *   10:45 AM, 1:35 PM, 4:20 PM, 7:05 PM
*   **Hamnet** (PG-13)
    *   12:20 PM, 3:25 PM, 6:30 PM
*   **The Housemaid** (R)
    *   2:00 PM, 5:00 PM, 8:00 PM, 10:15 PM
*   **Wicked: For Good**
    *   *Check theater for exact late-afternoon and evening showtimes.*

### **Special Holiday Events**
*   **How the Grinch Stole Christmas (25th Anniversary):** Screening as part of the Harkins Holiday Series.
*   **Harkins Holiday Series:** Various seasonal classics are playing through December 22.

---
**Theater Information:**
*   **Address:** 4751 East Marketplace Dr, Flagstaff, AZ 86004
*   **Phone:** (928) 233-3005

*Showtimes are subject to change. It is recommended to verify specific times on the official Harkins website or app before heading to the theater.*"
*  
```

## Using Google’s “Grounding Search” With Citations

This example is also near the bottom of the file **gemini.lisp** and adds the display of citations that consist of web sites searched. You can use the code in the web spidering chapter to fetch the text contents of these reference search results.

Here is the added code:

```lisp
(defun generate-with-search-and-citations (prompt &optional (model-id *model*))
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
           (escaped-json (escape-json data))
           (curl-cmd (format nil "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string (run-curl-command curl-cmd))
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

## Mixing Local Tools with Google Platform Tools Using the Interactions APIs.


The following diagram shows the high-level architecture of the Google Gemini API client library developed in this chapter:

{width: "80%"}
![Architecture diagram](images/gemini_architecture.png)

The Google Interactions APIs, as of March 2026, are in beta and may change. You can find the documentation here [https://ai.google.dev/gemini-api/docs/interactions](https://ai.google.dev/gemini-api/docs/interactions?ua=chat).

**Note: As of March 23 2026, I am still working on the Interactions example code. Latest code will be in GitHub repository in **loving-common-lisp/src/gemini/gemini_interactions_api.lisp**.

The following Common Lisp implementation in the file **gemini_interactions_api.lisp** offers a robust framework for interacting with Google’s Gemini API, specifically focusing on multi-turn conversations and the orchestration of client-side tool use. By leveraging the `cl-json` library for serialization, the code facilitates a seamless exchange between Lisp’s symbolic data structures and the JSON-based REST requirements of the Gemini endpoint. The core logic handles the intricate "Turn 1" and "Turn 2" workflow: it first dispatches a user prompt alongside function declarations, parses potential function calls requested by the model, and then provides a mechanism to feed the results of those local computations back to the model to generate a final, grounded response. Key utility functions within the package automate the conversion between Lisp’s hyphenated naming conventions and the API’s expected camelCase format, ensuring that internal hash-table representations are perfectly aligned with the schema required for tool configurations and content parts.

```lisp
(in-package #:gemini)

;;; ====================================================================
;;; Gemini Interactions API - Multi-turn conversations with tool use
;;; ====================================================================
;;;
;;; Supports multi-turn conversations combining Google Search (built-in)
;;; and custom function declarations (client-side tool use).
;;;
;;; Typical workflow:
;;;   1. Build function declarations with MAKE-FUNCTION-DECLARATION
;;;   2. Call GENERATE-WITH-TOOLS for Turn 1 -- returns TEXT, FUNCTION-CALLS, MODEL-CONTENT-HT
;;;   3. Invoke the functions yourself and collect results
;;;   4. Call CONTINUE-WITH-FUNCTION-RESPONSES for Turn 2 -- returns final TEXT
;;; ====================================================================


;;; ---- Internal utilities ----

(defun %symbol-name-to-camel-case (sym-name)
  "Converts a Lisp symbol name string (e.g. \"FUNCTION-CALL\") to camelCase (\"functionCall\").
   Consecutive hyphens (produced when cl-json decodes mixed snake_CamelCase keys) are collapsed."
  (let* ((raw-words (loop for start = 0 then (1+ pos)
                          for pos = (position #\- sym-name :start start)
                          collect (subseq sym-name start pos)
                          while pos))
         ;; Drop empty segments arising from consecutive hyphens (e.g. "SEARCH--SUGGESTIONS")
         (words (remove-if (lambda (w) (zerop (length w))) raw-words)))
    (if (null words)
        (string-downcase sym-name)
        (with-output-to-string (s)
          (write-string (string-downcase (first words)) s)
          (dolist (word (rest words))
            (write-char (char-upcase (char word 0)) s)
            (write-string (string-downcase (subseq word 1)) s))))))

(defun %is-decoded-alist-p (x)
  "Returns T if X looks like a cl-json decoded JSON object (alist with symbol keys)."
  (and (listp x) x (consp (first x)) (symbolp (caar x))))

(defun %decoded-to-ht (decoded)
  "Recursively converts a cl-json decoded value back to hash-tables for re-encoding.
   cl-json decodes JSON objects as alists with keyword keys (e.g. :FUNCTION-CALL).
   This inverts that, producing hash-tables with camelCase string keys so the
   value can be re-serialised faithfully with cl-json:encode-json-to-string."
  (cond
    ((%is-decoded-alist-p decoded)
     (let ((ht (make-hash-table :test 'equal)))
       (dolist (pair decoded ht)
         (let* ((key (%symbol-name-to-camel-case (symbol-name (car pair))))
                (val (%decoded-to-ht (cdr pair))))
           (setf (gethash key ht) val)))))
    ((listp decoded)
     (mapcar #'%decoded-to-ht decoded))
    (t decoded)))

(defun %make-content-ht (role parts)
  "Creates a content hash-table with role and parts list."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "role" ht) role
          (gethash "parts" ht) parts)
    ht))

(defun %make-text-part (text)
  "Creates a text part hash-table."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "text" ht) text)
    ht))

(defun %make-function-response-part (name id response-data)
  "Creates a functionResponse part hash-table.
   NAME: function name string
   ID: the function call ID returned by the model in Turn 1
   RESPONSE-DATA: the value to return as the function result (string, number, etc.)"
  (let ((resp-ht (make-hash-table :test 'equal))
        (fr-ht   (make-hash-table :test 'equal))
        (part-ht (make-hash-table :test 'equal)))
    ;; API expects: {"response": {"response": <data>}}
    (setf (gethash "response" resp-ht) response-data)
    (setf (gethash "name"     fr-ht) name
          (gethash "id"       fr-ht) id
          (gethash "response" fr-ht) resp-ht)
    (setf (gethash "functionResponse" part-ht) fr-ht)
    part-ht))

(defun %make-tools-list (function-declarations &optional google-search-p)
  "Builds the tools array for the API payload.
   FUNCTION-DECLARATIONS: list of hash-tables from MAKE-FUNCTION-DECLARATION, or NIL.
   GOOGLE-SEARCH-P: when T, prepends the built-in Google Search tool."
  (let ((tools '()))
    (when function-declarations
      (let ((fn-tool (make-hash-table :test 'equal)))
        (setf (gethash "functionDeclarations" fn-tool) function-declarations)
        (push fn-tool tools)))
    (when google-search-p
      (let ((gs-tool (make-hash-table :test 'equal)))
        (setf (gethash "googleSearch" gs-tool) (make-hash-table :test 'equal))
        (push gs-tool tools)))
    (nreverse tools)))

(defun %make-tool-config ()
  "Creates the toolConfig hash-table that asks the server to include its own tool invocations in the response."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "includeServerSideToolInvocations" ht) t)
    ht))

(defun %extract-function-calls (candidate)
  "Returns a list of plists (:NAME :ID :ARGS) for every functionCall part in CANDIDATE.
   ARGS is a cl-json decoded alist, e.g. ((:LOCATION . \"Barrow, AK\"))."
  (let* ((content (cdr (assoc :CONTENT candidate)))
         (parts   (cdr (assoc :PARTS content))))
    (loop for part in parts
          for fc-pair = (assoc :FUNCTION-CALL part)
          when fc-pair
          collect (let ((fc (cdr fc-pair)))
                    (list :name (cdr (assoc :NAME fc))
                          :id   (cdr (assoc :ID   fc))
                          :args (cdr (assoc :ARGS fc)))))))

(defun %get-text-from-candidate (candidate)
  "Returns the first text string found in CANDIDATE's parts, or NIL."
  (let* ((content (cdr (assoc :CONTENT candidate)))
         (parts   (cdr (assoc :PARTS content))))
    (loop for part in parts
          for text-pair = (assoc :TEXT part)
          when text-pair return (cdr text-pair))))


;;; ---- Public API ----

(defun make-function-declaration (name description parameters &optional required-params)
  "Creates a functionDeclaration hash-table suitable for GENERATE-WITH-TOOLS.

   NAME: string -- the function name the model will invoke
   DESCRIPTION: string -- natural-language description of what the function does
   PARAMETERS: list of (param-name type description) triples, e.g.:
     '((\"location\" \"STRING\" \"The city and state, e.g. San Francisco, CA\"))
   REQUIRED-PARAMS: optional list of required parameter name strings, e.g.:
     '(\"location\")

   Example:
     (make-function-declaration
       \"getWeather\"
       \"Get the weather in a given location\"
       '((\"location\" \"STRING\" \"The city and state, e.g. San Francisco, CA\"))
       '(\"location\"))"
  (let ((decl-ht  (make-hash-table :test 'equal))
        (params-ht (make-hash-table :test 'equal))
        (props-ht  (make-hash-table :test 'equal)))
    (dolist (param parameters)
      (destructuring-bind (pname ptype pdesc) param
        (let ((prop-ht (make-hash-table :test 'equal)))
          (setf (gethash "type"        prop-ht) ptype
                (gethash "description" prop-ht) pdesc)
          (setf (gethash pname props-ht) prop-ht))))
    (setf (gethash "type"       params-ht) "OBJECT"
          (gethash "properties" params-ht) props-ht)
    (when required-params
      (setf (gethash "required" params-ht) required-params))
    (setf (gethash "name"        decl-ht) name
          (gethash "description" decl-ht) description
          (gethash "parameters"  decl-ht) params-ht)
    decl-ht))

(defun generate-with-tools (prompt function-declarations
                            &key (model-id *model*) google-search-p)
  "Turn 1: Send PROMPT to the model with optional tool support.

   PROMPT: the user's text question.
   FUNCTION-DECLARATIONS: list of hash-tables from MAKE-FUNCTION-DECLARATION, or NIL.
   :GOOGLE-SEARCH-P: when T, enables the built-in Google Search tool.
   :MODEL-ID: model to use (defaults to *model*).

   Returns three values:
     TEXT             - model's text reply, or NIL when it chose to call functions.
     FUNCTION-CALLS   - list of plists (:NAME :ID :ARGS) for each function call made.
                        ARGS is a cl-json alist, e.g. ((:LOCATION . \"Barrow, AK\")).
     MODEL-CONTENT-HT - the model's content as a hash-table; pass this unchanged to
                        CONTINUE-WITH-FUNCTION-RESPONSES as the conversation history."
  (let* ((payload      (make-hash-table :test 'equal))
         (user-content (%make-content-ht "user" (list (%make-text-part prompt)))))
    (setf (gethash "contents"   payload) (list user-content)
          (gethash "tools"      payload) (%make-tools-list function-declarations google-search-p)
          (gethash "toolConfig" payload) (%make-tool-config))
    (let* ((api-url        (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data           (cl-json:encode-json-to-string payload))
           (escaped-json   (escape-json data))
           (curl-cmd       (format nil
                             "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string  (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates       (cdr (assoc :CANDIDATES decoded-response)))
           (candidate        (first candidates))
           (text             (%get-text-from-candidate candidate))
           (function-calls   (%extract-function-calls candidate))
           (model-content-ht (%decoded-to-ht (cdr (assoc :CONTENT candidate)))))
      (values text function-calls model-content-ht))))

(defun continue-with-function-responses (original-prompt model-content-ht
                                         function-responses function-declarations
                                         &key (model-id *model*) google-search-p)
  "Turn 2+: Continue the conversation by supplying function call results.

   ORIGINAL-PROMPT: the same user text string passed to GENERATE-WITH-TOOLS in Turn 1.
   MODEL-CONTENT-HT: the third return value from GENERATE-WITH-TOOLS (the model's content).
   FUNCTION-RESPONSES: list of plists with keys :NAME :ID :RESPONSE, one per function call, e.g.:
     (list (list :name \"getWeather\" :id \"call_123\" :response \"Very cold. 22F.\"))
     The :ID must match the :ID from the corresponding FUNCTION-CALLS entry returned by Turn 1.
   FUNCTION-DECLARATIONS: same list used in GENERATE-WITH-TOOLS.
   :GOOGLE-SEARCH-P: whether to include the Google Search tool (match Turn 1 setting).
   :MODEL-ID: model to use (defaults to *model*).

   Returns the model's final text response string."
  (let* ((payload         (make-hash-table :test 'equal))
         ;; Rebuild Turn-1 user message
         (user-content-1  (%make-content-ht "user" (list (%make-text-part original-prompt))))
         ;; Turn-1 model response (already a correctly shaped hash-table)
         ;; Turn-2 user message carrying the function results
         (fn-parts        (mapcar (lambda (fr)
                                    (%make-function-response-part
                                     (getf fr :name)
                                     (getf fr :id)
                                     (getf fr :response)))
                                  function-responses))
         (user-content-2  (%make-content-ht "user" fn-parts)))
    (setf (gethash "contents"   payload) (list user-content-1 model-content-ht user-content-2)
          (gethash "tools"      payload) (%make-tools-list function-declarations google-search-p)
          (gethash "toolConfig" payload) (%make-tool-config))
    (let* ((api-url        (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data           (cl-json:encode-json-to-string payload))
           (escaped-json   (escape-json data))
           (curl-cmd       (format nil
                             "curl -s -X POST ~A -H \"Content-Type: application/json\" -H \"x-goog-api-key: ~A\" -d \"~A\""
                             api-url *google-api-key* escaped-json))
           (response-string  (run-curl-command curl-cmd))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates       (cdr (assoc :CANDIDATES decoded-response)))
           (candidate        (first candidates)))
      (%get-text-from-candidate candidate))))
```

This implementation is structured around two primary public functions: **generate-with-tools** and **continue-with-function-responses**. The first function initiates the dialogue, accepting a natural language prompt and a list of tool definitions created via make-function-declaration. If the model determines that it needs more information than it currently possesses, such as real time weather data or a specific database query, then it returns a set of function calls. The programmer is then responsible for executing these functions locally and passing the outputs to the second function, which reconstructs the conversation history to provide the model with the context needed to conclude the interaction.

Under the hood, the code makes use of hash tables and recursion to manage the transformation of data. Because the Gemini API is highly sensitive to the structure of "parts" and "roles" within its content arrays, the helper functions `%make-content-ht` and `%make-function-response-part` ensure that the JSON payload is correctly nested. Furthermore, the inclusion of a dedicated `%symbol-name-to-camel-case` utility demonstrates a conservative and careful approach to Lisp integration, allowing developers to work with idiomatic Lisp symbols while maintaining strict compatibility with the external web service.

Here is sample code for testing:

```lisp
;;; ---- Usage example ----

;; 1. Define a custom function the model can request
(defparameter *get-weather-fn*
  (gemini:make-function-declaration
   "getWeather"
   "Get the weather in a given location"
   '(("location" "STRING" "The city and state, e.g. San Francisco, CA"))
   '("location")))

;; 2. Turn 1 -- send the question with tools enabled
(multiple-value-bind (text function-calls model-content)
    (gemini:generate-with-tools
     "What is the northernmost city in the United States? What's the weather like there today?"
     (list *get-weather-fn*)
     :google-search-p t)
  (format t "Text: ~A~%" text)
  (format t "Function calls: ~A~%" function-calls)

  ;; 3. If the model requested function calls, handle them and continue
  (when function-calls
    (let* ((fc (first function-calls))
           ;; In a real application you would call the actual weather API here.
           ;; The :ARGS plist contains ((:LOCATION . "Barrow, AK")) or similar.
           (weather-result "Very cold. 22 degrees Fahrenheit.")
           (fn-responses
            (list (list :name (getf fc :name)
                        :id   (getf fc :id)
                        :response weather-result))))

      ;; 4. Turn 2 -- provide the function results, get the final answer
      (let ((final-answer
             (gemini:continue-with-function-responses
              "What is the northernmost city in the United States? What's the weather like there today?"
              model-content
              fn-responses
              (list *get-weather-fn*)
              :google-search-p t)))
        (format t "~%Final answer: ~A~%" final-answer)))))
```

The output looks like:

```
Function calls: ((name getWeather id 04abw9d2 args
                  ((location . Utqiagvik, AK))))

Final answer: The northernmost city in the United States is **Utqiaġvik, Alaska** (formerly known as Barrow). 

As of today, the weather there is very cold with a temperature of **22°F** (-5.5°C). 

Located about 320 miles north of the Arctic Circle, Utqiaġvik is situated on the coast of the Arctic Ocean and experiences extreme conditions, including several weeks of total darkness in the winter and continuous daylight in the summer.
nil
```

