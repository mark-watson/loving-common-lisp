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

  (:export #:generate #:count-tokens #:send-chat-message #:generate-streaming #:generate-with-search #:generate-with-search-and-citations
           #:make-function-declaration #:generate-with-tools #:continue-with-function-responses))
```

### gemini.asd

```lisp
;;;; gemini.asd

(asdf:defsystem #:gemini
  :description "Library for using the Google Gemini Interactions API"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:alexandria #:dexador)
  :components ((:file "package")
               (:file "gemini")
	       (:file "gemini_interactions_api")))


```
### gemini.lisp

This code defines functions for generating content and counting tokens. Rather than spawning an external `curl` process, it executes native HTTP POST requests using the `Dexador` client via the helper function `%post-json`. 

To generate text, the `generate` function calls the Google Interactions API at the `https://generativelanguage.googleapis.com/v1beta/interactions` endpoint. It sends a simple JSON payload containing the model identifier and prompt string, then decodes the response and extracts the generated text from the steps. The `count-tokens` function queries the standard models endpoint (`countTokens` action) using the same native HTTP post helper.

```lisp
(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *interactions-api-url*
  "https://generativelanguage.googleapis.com/v1beta/interactions")

(defvar *model* "gemini-3-flash-preview") ;; model used in this file.

(defun %post-json (url headers payload-hash)
  "Helper function to perform an HTTP POST request with a JSON payload using Dexador."
  (let ((payload-json (cl-json:encode-json-to-string payload-hash)))
    (dex:post url :headers headers :content payload-json)))

;;; ---- Interactions API helpers ----

(defun %extract-text-from-steps (decoded-response)
  "Extract the text from the last model_output step in an Interactions API response.
   Response format: {\"steps\": [{\"type\": \"model_output\", \"content\": [{\"type\": \"text\", \"text\": \"...\"}]}]}"
  (let* ((steps (cdr (assoc :STEPS decoded-response))))
    (loop for step in (reverse steps)
          when (string-equal (cdr (assoc :TYPE step)) "model_output")
          return (let* ((content (cdr (assoc :CONTENT step)))
                        (first-content (first content)))
                   (cdr (assoc :TEXT first-content))))))

(defun generate (prompt &optional (model-id *model*))
  "Generates text from a given prompt using the Interactions API.
   Uses *model* defined at the top of this file as default.
   PROMPT: The text prompt to generate content from.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the generated text as a string."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt)
    (let* ((headers (list '("Content-Type" . "application/json")
                          (cons "x-goog-api-key" *google-api-key*)
                          '("Api-Revision" . "2026-05-20")))
           (response-string (%post-json *interactions-api-url* headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string)))
      (%extract-text-from-steps decoded-response))))
  
;; (gemini:generate "In one sentence, explain how AI works to a child.")
;; (gemini:generate "Write a short, four-line poem about coding in Python.")

(defun count-tokens (prompt &optional (model-id *model*))
  "Counts the number of tokens for a given prompt and model.
   Uses *model* defined at top of this file as default.
   PROMPT: The text prompt to count tokens for.
   MODEL-ID: Optional. The ID of the model to use.
   Returns the total token count as an integer."
  (let* ((api-url (concatenate 'string
                               "https://generativelanguage.googleapis.com/v1beta/models/"
                               model-id ":countTokens"))
         (payload (make-hash-table :test 'equal)))
    ;; Construct payload similar to generate function
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((headers (list '("Content-Type" . "application/json")
                          (cons "x-goog-api-key" *google-api-key*)))
           (response-string (%post-json api-url headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string))
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
  "Generates text with Google Search grounding via the Interactions API."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt
          (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "type" tool) "google_search")
                  tool)))
    (let* ((headers (list '("Content-Type" . "application/json")
                          (cons "x-goog-api-key" *google-api-key*)
                          '("Api-Revision" . "2026-05-20")))
           (response-string (%post-json *interactions-api-url* headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string)))
      (%extract-text-from-steps decoded-response))))

;; (gemini:generate-with-search "Consultant Mark Watson has written Common Lisp, semantic web, Clojure, Java, and AI books. What musical instruments does he play?")
```

The core mechanism of this implementation relies on the payload hash table construction, specifically where the `"tools"` key is populated. Unlike a standard generation request, this payload includes a list containing a Google Search tool definition; the presence of this specific configuration acts as a switch, granting the model permission to query Google's search index before formulating its answer. This is particularly critical for questions regarding current events, as the model's static training data would otherwise be outdated.

Because we are calling the `/v1beta/interactions` API rather than standard generation endpoints, the payload accepts a direct `"input"` prompt string, and the response is structured as a series of unified `"steps"`. The helper `%extract-text-from-steps` simplifies traversal by parsing the returned steps in reverse order and returning the text from the last `model_output` step.

Here is example output:

```lisp
$ sbcl
This is SBCL 2.5.10, an implementation of ANSI Common Lisp.
* (ql:quickload :gemini)
To load "gemini":
  Load 1 ASDF system:
     gemini
; Loading "gemini"
nil
* (gemini:generate-with-search "What movies are playing in Flagstaff Arizona today at the Harkins Theater?")
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
...
"
```

## Using Google’s “Grounding Search” With Citations

This example is also near the bottom of the file **gemini.lisp** and adds the retrieval and display of citations that consist of web sites searched. You can use the code in the web spidering chapter to fetch the text contents of these reference search results.

Here is the code:

```lisp
(defun generate-with-search-and-citations (prompt &optional (model-id *model*))
  "Generates text with Google Search grounding and returns citations via the Interactions API.
   Returns two values: the response text and a list of (title . url) citation pairs."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt
          (gethash "tools" payload)
          (list (let ((tool (make-hash-table :test 'equal)))
                  (setf (gethash "type" tool) "google_search")
                  tool)))
    (let* ((headers (list '("Content-Type" . "application/json")
                          (cons "x-goog-api-key" *google-api-key*)
                          '("Api-Revision" . "2026-05-20")))
           (response-string (%post-json *interactions-api-url* headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (steps (cdr (assoc :STEPS decoded-response)))
           ;; Extract text from last model_output step
           (text (loop for step in (reverse steps)
                       when (string-equal (cdr (assoc :TYPE step)) "model_output")
                       return (let* ((content (cdr (assoc :CONTENT step)))
                                     (first-content (first content)))
                                 (cdr (assoc :TEXT first-content)))))
           ;; Extract citations from url_citation annotations in model_output steps
           (citations
            (loop for step in steps
                  when (string-equal (cdr (assoc :TYPE step)) "model_output")
                  append (loop for content-item in (cdr (assoc :CONTENT step))
                               append (loop for annotation in (cdr (assoc :ANNOTATIONS content-item))
                                            when (string-equal (cdr (assoc :TYPE annotation)) "url_citation")
                                            collect (cons (cdr (assoc :TITLE annotation))
                                                          (cdr (assoc :URL annotation))))))))
      ;; Return both text and citations
      (values text citations))))
```

Here is the sample output (output shortened: redirect URIs shortened for brevity):

```lisp
* (multiple-value-bind (response sources)
    (gemini:generate-with-search-and-citations "Who won the Super Bowl in 2024?")
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
![Architecture diagram](images/gThe Google Interactions APIs provide a clean schema for managing multi-turn conversations and executing client-side tools. You can find more details in the official documentation here: [https://ai.google.dev/gemini-api/docs/interactions](https://ai.google.dev/gemini-api/docs/interactions?ua=chat).

The following Common Lisp implementation in the file **gemini_interactions_api.lisp** offers a robust framework for multi-turn conversations with tool orchestration. Unlike standard API generation endpoints where the developer has to manually accumulate and format the message history on the client side, the Interactions endpoint maintains the conversation state server-side. 

When you call `generate-with-tools` on "Turn 1," the API returns the model's output alongside an `interaction-id` that references the server-side conversation state. If the model determines it needs to call a client-side function (tool), it returns a list of function calls. You execute those functions locally and pass the results to `continue-with-function-responses` on "Turn 2" alongside the same `interaction-id`. The server matches the results to the pending interaction context, allowing it to formulate the final answer.

```lisp
(in-package #:gemini)

;;; ====================================================================
;;; Gemini Interactions API - Multi-turn conversations with tool use
;;; ====================================================================
;;;
;;; Uses the v1beta Interactions API with the new steps schema.
;;; Supports multi-turn conversations combining Google Search (built-in)
;;; and custom function declarations (client-side tool use).
;;;
;;; Typical workflow:
;;;   1. Build function declarations with MAKE-FUNCTION-DECLARATION
;;;   2. Call GENERATE-WITH-TOOLS for Turn 1 -- returns TEXT, FUNCTION-CALLS, INTERACTION-ID
;;;   3. Invoke the functions yourself and collect results
;;;   4. Call CONTINUE-WITH-FUNCTION-RESPONSES for Turn 2 -- returns final TEXT
;;; ====================================================================


;;; ---- Internal utilities ----

(defun %make-tools-list (function-declarations &optional google-search-p)
  "Builds the tools array for the Interactions API payload.
   FUNCTION-DECLARATIONS: list of hash-tables from MAKE-FUNCTION-DECLARATION, or NIL.
   GOOGLE-SEARCH-P: when T, includes the built-in Google Search tool."
  (let ((tools '()))
    (when function-declarations
      (loop for fn-decl in function-declarations
            do (let ((fn-tool (make-hash-table :test 'equal)))
                 (setf (gethash "type" fn-tool) "function"
                       (gethash "name" fn-tool) (gethash "name" fn-decl)
                       (gethash "description" fn-tool) (gethash "description" fn-decl)
                       (gethash "parameters" fn-tool) (gethash "parameters" fn-decl))
                 (push fn-tool tools))))
    (when google-search-p
      (let ((gs-tool (make-hash-table :test 'equal)))
        (setf (gethash "type" gs-tool) "google_search")
        (push gs-tool tools)))
    (nreverse tools)))

(defun %extract-function-calls-from-steps (steps)
  "Returns a list of plists (:NAME :ID :ARGS) for every function_call step.
   ARGS is a cl-json decoded alist, e.g. ((:LOCATION . \"Barrow, AK\"))."
  (loop for step in steps
        when (string-equal (cdr (assoc :TYPE step)) "function_call")
        collect (list :name (cdr (assoc :NAME step))
                      :id   (cdr (assoc :ID   step))
                      :args (cdr (assoc :ARGUMENTS step)))))

(defun %get-text-from-steps (steps)
  "Returns the text from the last model_output step, or NIL."
  (loop for step in (reverse steps)
        when (string-equal (cdr (assoc :TYPE step)) "model_output")
        return (let* ((content (cdr (assoc :CONTENT step)))
                      (first-content (first content)))
                 (cdr (assoc :TEXT first-content)))))


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
  "Turn 1: Send PROMPT to the model with optional tool support via the Interactions API.

   PROMPT: the user's text question.
   FUNCTION-DECLARATIONS: list of hash-tables from MAKE-FUNCTION-DECLARATION, or NIL.
   :GOOGLE-SEARCH-P: when T, enables the built-in Google Search tool.
   :MODEL-ID: model to use (defaults to *model*).

   Returns three values:
     TEXT             - model's text reply, or NIL when it chose to call functions.
     FUNCTION-CALLS   - list of plists (:NAME :ID :ARGS) for each function call made.
                        ARGS is a cl-json alist, e.g. ((:LOCATION . \"Barrow, AK\")).
     INTERACTION-ID   - the interaction ID for use in follow-up turns."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt
          (gethash "tools" payload) (%make-tools-list function-declarations google-search-p))
    (let* ((headers (list '("Content-Type" . "application/json")
                          (cons "x-goog-api-key" *google-api-key*)
                          '("Api-Revision" . "2026-05-20")))
           (response-string  (%post-json *interactions-api-url* headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (steps            (cdr (assoc :STEPS decoded-response)))
           (interaction-id   (cdr (assoc :ID decoded-response)))
           (text             (%get-text-from-steps steps))
           (function-calls   (%extract-function-calls-from-steps steps)))
      (values text function-calls interaction-id))))

(defun continue-with-function-responses (interaction-id
                                         function-responses function-declarations
                                         &key (model-id *model*) google-search-p)
  "Turn 2+: Continue an interaction by supplying function call results.

   INTERACTION-ID: the interaction ID from GENERATE-WITH-TOOLS.
   FUNCTION-RESPONSES: list of plists with keys :NAME :ID :RESPONSE, one per function call, e.g.:
     (list (list :name \"getWeather\" :id \"fc_123\" :response \"Very cold. 22F.\"))
     The :ID must match the :ID from the corresponding FUNCTION-CALLS entry returned by Turn 1.
   FUNCTION-DECLARATIONS: same list used in GENERATE-WITH-TOOLS.
   :GOOGLE-SEARCH-P: whether to include the Google Search tool (match Turn 1 setting).
   :MODEL-ID: model to use (defaults to *model*).

   Returns the model's final text response string."
  (let* ((payload (make-hash-table :test 'equal))
         (fn-results (mapcar (lambda (fr)
                               (let ((result-ht (make-hash-table :test 'equal))
                                     (text-block (make-hash-table :test 'equal)))
                                 (setf (gethash "type" text-block) "text"
                                       (gethash "text" text-block) (getf fr :response))
                                 (setf (gethash "type" result-ht) "function_result"
                                       (gethash "name" result-ht) (getf fr :name)
                                       (gethash "call_id" result-ht) (getf fr :id)
                                       (gethash "result" result-ht) (list text-block))
                                 result-ht))
                             function-responses)))
    (setf (gethash "model" payload) model-id
          (gethash "previous_interaction_id" payload) interaction-id
          (gethash "input" payload) fn-results
          (gethash "tools" payload) (%make-tools-list function-declarations google-search-p))
    (let* ((headers (list '("Content-Type" . "application/json")
                          (cons "x-goog-api-key" *google-api-key*)
                          '("Api-Revision" . "2026-05-20")))
           (response-string  (%post-json *interactions-api-url* headers payload))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (steps            (cdr (assoc :STEPS decoded-response)))
           (new-interaction-id (cdr (assoc :ID decoded-response)))
           (text             (%get-text-from-steps steps))
           (function-calls   (%extract-function-calls-from-steps steps)))
      (values text function-calls new-interaction-id))))
```

This implementation is structured around two primary public functions: **generate-with-tools** and **continue-with-function-responses**. The first function initiates the dialogue, accepting a natural language prompt and a list of tool definitions created via `make-function-declaration`. If the model determines that it needs more information than it currently possesses, such as real-time weather data or a specific database query, it returns a list of function calls. The programmer is then responsible for executing these functions locally and passing the outputs to the second function, which targets the same interaction context using the `interaction-id` to provide the model with the context needed to conclude the interaction.

Under the hood, the helper `%make-tools-list` builds a tool declaration matching the expected structure of the Interactions API. The helper `%extract-function-calls-from-steps` extracts the model's requested function calls and returns them as a structured Lisp property list.

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
(multiple-value-bind (text function-calls interaction-id)
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
              interaction-id
              fn-responses
              (list *get-weather-fn*)
              :google-search-p nil)))
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

## Optional Practice Problems

1. **Prompt Comparison:** Write a function `compare-prompts` that accepts two prompt strings, calls `gemini:generate` on each, and prints both results side-by-side with labels. Use `gemini:count-tokens` to also display the token count for each prompt before generating. This exercise reinforces basic usage of the `generate` and `count-tokens` API functions.

2. **Token Budget Helper:** Write a function `fits-in-budget-p` that accepts a prompt string and a maximum token integer. It should call `gemini:count-tokens` and return `T` if the prompt fits within the budget, or `NIL` otherwise. Then write a wrapper around `gemini:generate` called `generate-if-within-budget` that only calls the API when the prompt is within a given token limit, printing a warning message instead of calling the API when the budget is exceeded.

3. **Search-Grounded Fact Checker:** Using `gemini:generate-with-search`, write a function `fact-check` that takes a factual claim as a string, prepends the instruction "Is the following claim true or false? Provide evidence:", and returns the grounded response. Compare the output of `fact-check` with calling plain `gemini:generate` on the same claim to observe how search grounding improves factual accuracy.

4. **Citation Formatter:** Write a function `print-sourced-answer` that wraps `gemini:generate-with-search-and-citations`. It should use `multiple-value-bind` to capture both the response text and the citations list, then print the answer followed by a numbered bibliography of sources. Each source line should display its index, title, and URL using `format`. This exercise practices working with Common Lisp multiple return values.

5. **Custom Tool: Calculator:** Using `gemini:make-function-declaration`, define a `calculate` tool that accepts two parameters: `expression` (a string like "2 + 3") and `precision` (an integer for decimal places). Write the full two-turn interaction: call `gemini:generate-with-tools` with a math question, parse the returned function call from the `:ARGS` alist, evaluate the expression locally (you can use a simple `read-from-string` and `eval` approach), and pass the result back via `gemini:continue-with-function-responses` to get the model's final answer.

6. **Multi-Tool Agent:** Extend the weather tool example from the chapter by registering two function declarations: `getWeather` (as shown) and a new `getPopulation` that takes a `city` parameter. Write a single prompt that asks about both the weather and the population of a city. Observe how the model returns multiple entries in the `function-calls` list, and write code that iterates over all of them, builds simulated responses for each, and passes the full list to `continue-with-function-responses` in a single Turn 2 call.
