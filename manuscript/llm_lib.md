# New LLM Library With Tool Support

Dear reader, as I write this chapter in March 2026 I already have six chapters in this book covering the use of LLMs from different providers. I experiment a lot rewriting code, and I have a new library included in the GitHub repository for this book called **llm** that is the result of spending the last week refactoring my older code and adding new functionality with the goal of having a small library that supports Ollama local models, Anthropic Claude APIs, and Google Gemini APIs with similar functionality except for adding support for Google's integrated web search and Gemini APIs.

## New LLM Library With Tool Support


Dear reader, as I write this chapter in March 2026 I already have six chapters in this book covering the use of LLMs from different providers. I experiment a lot rewriting code, and I have a new library included in the GitHub repository for this book called llm that is the result of spending the last week refactoring my older code and adding new functionality with the goal of having a small library that supports Ollama local models, Anthropic Claude APIs, and Google Gemini APIs with similar functionality except for adding support for Google's integrated web search and Gemini APIs.

### Library Structure Overview

The llm library is organized into three focused source files that work together: llm.lisp provides shared low-level utilities, simple-tools.lisp defines a provider-agnostic tool registry and schema system, and provider-specific files like claude.lisp and ollama.lisp implement the actual API calls. This separation of concerns makes it straightforward to add new providers without touching the tool infrastructure.

### Source Code

The **llm** library code is in the book Github repository in the directory **loving-common-lisp/src/llm**.

The test code is in the directory **loving-common-lisp/src/llm_test**.

### llm.lisp — Shared Utilities

```lisp
(defpackage #:llm
  (:use #:cl)
  (:export #:run-curl-command
           #:escape-json
           #:substitute-subseq))
(in-package #:llm)
(defun run-curl-command (curl-command)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program curl-command
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (if (zerop exit-code)
        output
        (error "Curl command failed: ~A~%Error: ~A" curl-command error-output))))
(defun escape-json (str)
  (with-output-to-string (out)
    (loop for ch across str do
          (if (char= ch #\")
              (write-string "\\\"" out)
              (if (char= ch #\\)
                  (write-string "\\\\" out)
                  (write-char ch out))))))
(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))
```

This utility package exports just three functions. run-curl-command shells out to curl via UIOP and returns the response body as a string, signaling an error if the exit code is non-zero. escape-json walks a string character by character and escapes double-quotes and backslashes so the JSON payload can be safely embedded inside a shell double-quoted string argument — a necessary workaround when building curl commands this way. Finally, substitute-subseq does a single-pass string substitution; it is used to replace :null with :false in serialized JSON, which corrects a quirk in how cl-json renders nil values that would otherwise confuse the LLM APIs.

### simple-tools.lisp — Provider-Agnostic Tool Registry

```lisp
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
(defpackage #:simple-tools
  (:use #:cl)
  (:export #:*tools*
           #:tool
	   #:define-tool
           #:make-tool
           #:tool-name
           #:tool-description
           #:tool-parameters
           #:tool-fn
           #:define-tool
           #:call-tool
           #:render-tool
           #:map-args-to-parameters))
(in-package #:simple-tools)
(defstruct tool
  "A tool callable by LLM completions."
  name
  description
  parameters
  fn)
(defvar *tools* (make-hash-table :test 'equalp))
(defmacro define-tool (name args description &body body)
  "Define a tool callable by LLM completions.
ARGS is a list of (param-name type description) triples."
  (let ((name-str (if (symbolp name) (string-downcase (symbol-name name)) name))
        (arg-names (mapcar (lambda (arg) (intern (string-upcase (first arg)))) args)))
    `(setf (gethash ,name-str *tools*)
           (make-tool
            :name ,name-str
            :description ,description
            :parameters ',args
            :fn (lambda ,arg-names ,@body)))))
(defun call-tool (tool-name &rest args)
  "Call a tool by name with the given arguments."
  (let ((tool (gethash tool-name *tools*)))
    (if tool
        (apply (tool-fn tool) args)
        (error "Unknown tool: ~A" tool-name))))
(defun render-tool (tool)
  "Render a tool as a JSON schema alist for LLM API requests."
  `((:type . "function")
    (:function . ,(remove nil
                    `((:name . ,(tool-name tool))
                      (:description . ,(tool-description tool))
                      ,(when (tool-parameters tool)
                         `(:parameters . ((:type . "object")
                                          (:properties . ,(loop for p in (tool-parameters tool)
                                                                collect (list (first p)
                                                                              (cons :type (second p))
                                                                              (cons :description (third p)))))
                                          (:required . ,(loop for p in (tool-parameters tool)
                                                              collect (first p)))))))))))
(defun map-args-to-parameters (tool args)
  "Map an alist of JSON arguments to positional values in the tool's declared parameter order."
  (loop for (param-name param-type param-desc) in (tool-parameters tool)
        collect (rest (assoc (intern (string-upcase param-name) :keyword) args))))
```

The simple-tools package is the heart of the tool support system. Tools are defined with the define-tool macro, which takes a name, a list of parameter triples of the form (param-name type description), a docstring description, and a body. Internally each tool is stored as a struct in the *tools* hash table, keyed by its lowercase string name. This means tools defined once are immediately available to all provider backends — you define a tool in one place and can pass it by name to claude:completions, ollama:completions, or any future Gemini wrapper without modification.
The render-tool function serializes a tool into the OpenAI-compatible JSON schema format used by Ollama. Claude's API uses a slightly different schema key (input_schema rather than parameters), so the claude.lisp file provides its own render-tool-for-claude function that produces the correct structure while still reading from the same *tools* registry.
map-args-to-parameters handles the impedance mismatch between the alist of named arguments returned by the JSON parser and the positional argument list expected by the tool's underlying lambda. It walks the tool's declared parameter list in order and looks up each parameter by its keyword-interned name in the response alist, returning an ordered list of values ready for apply.

### claude.lisp — Anthropic Claude Backend

```lisp
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(defpackage #:claude
  (:use #:cl #:llm)
  (:export #:claude-llm
           #:completions
           #:answer-question))

(in-package #:claude)

(defvar *claude-endpoint* "https://api.anthropic.com/v1/messages")
(defvar *claude-model* "claude-sonnet-4-6")
(defvar *claude-max-tokens* 1000)

(defun get-claude-api-key ()
  (uiop:getenv "CLAUDE_API"))

(defun render-tool-for-claude (tool)
  "Render a tool as a JSON schema alist in Claude's input_schema format."
  (let* ((params (simple-tools:tool-parameters tool))
         (properties (loop for p in params
                           collect (let ((desc (third p)))
                                     (if desc
                                         (list (first p)
                                               (cons :type (second p))
                                               (cons :description desc))
                                         (list (first p)
                                               (cons :type (second p)))))))
         (required (loop for p in params collect (first p)))
         (schema (append '((:type . "object"))
                         (when properties (list (cons :properties properties)))
                         (when required (list (cons :required required))))))
    `((:name . ,(simple-tools:tool-name tool))
      (:description . ,(simple-tools:tool-description tool))
      (:input--schema . ,schema))))

(defun completions (starter-text &optional tools (model-id *claude-model*))
  (let* ((tools-rendered (when tools
                           (loop for tool-symbol in tools
                                 collect (let ((tool (gethash (string tool-symbol) simple-tools:*tools*)))
                                           (if tool
                                               (render-tool-for-claude tool)
                                               (error "Undefined tool function: ~A" tool-symbol))))))
         (messages (cond
                     ((stringp starter-text)
                      (list `((:role . "user")
                              (:content . (((:type . "text") (:text . ,starter-text)))))))
                     (t starter-text)))
         (base-data `((:model . ,model-id)
                      (:max--tokens . ,*claude-max-tokens*)
                      (:temperature . 0)
                      (:messages . ,messages)))
         (data (if tools-rendered
                   (append base-data (list (cons :tools tools-rendered)))
                   base-data))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data (llm:substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (llm:escape-json fixed-json-data))
         (curl-command (format nil "curl ~A -H \"x-api-key: ~A\" -H \"anthropic-version: 2023-06-01\" -H \"content-type: application/json\" -d \"~A\""
                               *claude-endpoint*
                               (get-claude-api-key)
                               escaped-json)))
    (format t "$$ data:~%~A~%" data)
    (let ((response (llm:run-curl-command curl-command)))
      (with-input-from-string (s response)
        (let* ((json-as-list (cl-json:decode-json s))
               (content (cdr (assoc :content json-as-list)))
               (stop-reason (cdr (assoc :stop--reason json-as-list)))
               (tool-use-blocks (when (string= stop-reason "tool_use")
                                  (remove-if-not (lambda (block)
                                                   (string= (cdr (assoc :type block)) "tool_use"))
                                                 content))))
          (if tool-use-blocks
              (let ((results
                     (loop for block in tool-use-blocks
                           collect (let* ((name (cdr (assoc :name block)))
                                         (input (cdr (assoc :input block)))
                                         (tool (gethash name simple-tools:*tools*))
                                         (mapped-args (simple-tools:map-args-to-parameters tool input)))
                                     (apply (simple-tools:tool-fn tool) mapped-args)))))
                (format nil "~{~A~^~%~}" results))
              (let ((first-block (car content)))
                (or (cdr (assoc :text first-block)) "No response content"))))))))

(defun answer-question (question)
  (completions (concatenate 'string "Concisely answer the question: " question)))
```

The claude.lisp backend differs from the Ollama backend in a few notable ways. First, the API key is read from the CLAUDE_API environment variable and passed as an x-api-key header alongside an anthropic-version header, both required by Anthropic's API. Second, because Claude's messages API wraps content in typed blocks, plain string input is converted into the structured message format {role: "user", content: [{type: "text", text: "..."}]} before serialization, while a pre-built message list is passed through unchanged, giving callers flexibility for multi-turn conversations.
Tool schema rendering diverges from the Ollama format: Claude expects a top-level :input_schema key (rendered as :input--schema to satisfy cl-json's hyphen-to-double-hyphen convention) directly on the tool object rather than nesting under a :function key. The stop reason detection also differs — Claude signals tool use via a stop_reason field set to "tool_use" in the response, at which point the code filters the content array for blocks of type "tool_use", dispatches each to the corresponding function in *tools*, and concatenates the results.

### ollama.lisp — Local Ollama Backend

```lisp
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
(defpackage #:ollama
  (:use #:cl #:llm)
  (:export #:ollama-llm
           #:completions
           #:summarize
           #:answer-question))
(in-package #:ollama)
(defvar *ollama-endpoint* "http://localhost:11434/api/chat")
(defvar *ollama-model* "mistral:v0.3")
(defun completions (starter-text &optional tools (model-id *ollama-model*))
  (let* ((tools-rendered
          (when tools
            (loop for tool-symbol in tools
                  collect (let ((tool (gethash (string tool-symbol) simple-tools:*tools*)))
                            (if tool
                                (simple-tools:render-tool tool)
                                (error "Undefined tool function: ~A" tool-symbol))))))
         (message (list (cons :|role| "user")
                        (cons :|content| starter-text)))
         (data (list (cons :|model| model-id)
                     (cons :|stream| nil)
                     (cons :|messages| (list message))))
         (data-with-tools (if tools-rendered
                              (append data (list (cons :|tools| tools-rendered)))
                              data))
         (json-data (cl-json:encode-json-to-string data-with-tools))
         (fixed-json-data
          (llm:substitute-subseq json-data ":null" ":false" :test #'string=))
         (escaped-json (llm:escape-json fixed-json-data))
         (curl-command (format nil "curl ~a -d \"~A\""
                               *ollama-endpoint*
                               escaped-json)))
    (let ((response (llm:run-curl-command curl-command)))
      (with-input-from-string (s response)
        (let* ((json-as-list (cl-json:decode-json s))
               (message-resp (cdr (assoc :message json-as-list)))
               (tool-calls (cdr (assoc :tool--calls message-resp)))
               (content (cdr (assoc :content message-resp))))
          (if tool-calls
              (let ((results
                     (loop for call in tool-calls
                           collect (let* ((func (cdr (assoc :function call)))
                                         (name (cdr (assoc :name func)))
                                         (args (cdr (assoc :arguments func)))
                                         (tool (gethash name simple-tools:*tools*))
                                         (mapped-args (simple-tools:map-args-to-parameters tool args)))
                                     (apply (simple-tools:tool-fn tool) mapped-args)))))
                (format nil "~{~A~^~%~}" results))
              (or content "No response content")))))))
(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))
(defun answer-question (some-text)
  (completions (concatenate 'string "Q: " some-text " A:")))
```

The Ollama backend targets a locally running Ollama server on localhost:11434 and defaults to mistral:v0.3, though any model name supported by your local Ollama installation can be passed as the optional third argument. Unlike the Claude backend, no authentication header is needed. The request format follows the OpenAI chat completions convention that Ollama implements, so simple-tools:render-tool (which produces the {type: "function", function: {...}} shape) is used directly without a custom renderer. Tool call detection reads from message.tool_calls in the response rather than from a top-level stop_reason, reflecting the structural difference between the two APIs. The package also exports convenience wrappers summarize and answer-question that prepend simple prompt prefixes.

### Defining Tools — example_tools.lisp

```lisp
(ql:quickload :llm)
(defpackage #:example-tools
  (:use #:cl #:simple-tools))
(in-package #:example-tools)
(define-tool get-weather
    ((location string "The city and state, e.g. San Francisco, CA")
     (unit string "The unit of temperature, e.g. 'c' or 'f'"))
    "Get the current weather in a given location"
  (format t "~%[TOOL EXECUTION] Getting weather for ~A in ~A~%" location unit)
  (if (equal unit "c")
      "22"
      "72"))
(define-tool add-numbers ((a number) (b number))
  "Add two numbers together"
  (format nil "The sum of ~A and ~A is ~A" a b (+ a b)))
(define-tool get-current-time ()
  "Get the current time"
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "Current time: ~2,'0D:~2,'0D:~2,'0D on ~2,'0D/~2,'0D/~A"
            hour min sec month date year)))
(define-tool capitalize-text ((text string))
  "Convert text to uppercase"
  (format nil "Capitalized: ~A" (string-upcase text)))
```

This file demonstrates the define-tool macro in practice. Notice that get-weather stubs out real weather data — it simply returns "22" for Celsius and "72" for Fahrenheit — but the important point is the dispatch mechanism: the LLM correctly identifies which tool to call and with what arguments based solely on the natural language query and the tool's description and parameter metadata. add-numbers and get-current-time show that tools can have purely numeric parameters or no parameters at all, and capitalize-text shows a simple single-parameter string tool. All four end up in the *tools* hash table under their lowercase string names the moment the file is loaded.

### Testing the Claude Backend — claude_test.lisp

```lisp
(load (merge-pathnames "example_tools.lisp"
                       (or *load-pathname* *default-pathname-defaults*)))
(defpackage #:claude-test
  (:use #:cl #:llm #:claude))
(in-package #:claude-test)
(format t "~%--- Testing Claude Completions ---~%")
(let ((response (claude:completions "Why is the sky blue?")))
  (format t "Response:~%~A~%" response))
(format t "~%--- Testing Claude Tool Calling ---~%")
(let ((response (claude:completions "What is the weather in Paris, France in celsius?" '("get-weather"))))
  (format t "Response:~%~A~%" response))
(format t "~%--- Testing Claude Tool Calling: add-numbers ---~%")
(let ((response (claude:completions "What is 42 plus 58?" '("add-numbers"))))
  (format t "Response:~%~A~%" response))
(format t "~%--- Testing Claude Tool Calling: get-current-time ---~%")
(let ((response (claude:completions "What is the current time?" '("get-current-time"))))
  (format t "Response:~%~A~%" response))
(format t "~%--- Testing Claude Tool Calling: capitalize-text ---~%")
(let ((response (claude:completions "Please capitalize the text 'hello world'" '("capitalize-text"))))
  (format t "Response:~%~A~%" response))
```

The test file loads the example tools first to populate *tools*, then exercises claude:completions in two modes: without tools (the plain sky-blue question) and with a single named tool passed as a one-element list of strings. Each tool name string must exactly match the key registered in *tools* by define-tool, which uses string-downcase on the symbol name — so '("get-weather") matches the tool defined as get-weather. Running this file with a valid CLAUDE_API environment variable should produce five blocks of output showing both a normal text completion and four successful tool dispatches.

### Design Notes and Tradeoffs

The decision to shell out to curl rather than using a native HTTP client library keeps the dependency footprint minimal. The tradeoff is that JSON must be escaped for shell embedding, which is what llm:escape-json handles. For production use you would likely want to replace the curl layer with dexador or usocket-based HTTP, but for experimentation and book examples the curl approach is refreshingly transparent — you can paste the printed curl command directly into a terminal to inspect the raw API interaction.
One limitation of the current tool dispatch is that only a single round-trip is performed. If the model's tool call result should be fed back to the model for a follow-up response (the full agentic loop), the caller must manage that conversation state manually by building a message list and passing it as starter-text. The completions function's acceptance of either a plain string or a pre-built message list makes this possible, and it is a natural extension to explore in the next chapter when we look at multi-step agent loops.

### Example Program Output

I left debug printout in my code that always starts with two dollar signs. I also edited some output for brevity:

```bash
$ sbcl
This is SBCL 2.5.10, an implementation of ANSI Common Lisp.
* (load "claude_test.lisp")
To load "llm":
  Load 1 ASDF system:
    llm
; Loading "llm"


--- Testing Claude Completions ---
$$ data:
((model . claude-sonnet-4-6) (max--tokens . 1000) (temperature . 0)
 (messages
  ((role . user) (content ((type . text) (text . Why is the sky blue?))))))
Response:
## Why the Sky is Blue

The sky appears blue because of a phenomenon called **Rayleigh scattering**.

### Here's how it works:

1. **Sunlight contains all colors** - White sunlight is made up of all the colors of the rainbow, each with different wavelengths.

  ...
  

--- Testing Claude Tool Calling ---
$$ data:
((model . claude-sonnet-4-6) (max--tokens . 1000) (temperature . 0)
 (messages
  ((role . user)
   (content
    ((type . text)
     (text . What is the weather in Paris, France in celsius?)))))
 (tools
  ((name . get-weather)
   (description . Get the current weather in a given location)
   (input--schema (type . object)
    (properties
     (location (type . string)
      (description . The city and state, e.g. San Francisco, CA))
     (unit (type . string)
      (description . The unit of temperature, e.g. 'c' or 'f')))
    (required location unit)))))

[TOOL EXECUTION] Getting weather for Paris, France in c
Response:
22

--- Testing Claude Tool Calling: add-numbers ---
$$ data:
((model . claude-sonnet-4-6) (max--tokens . 1000) (temperature . 0)
 (messages
  ((role . user) (content ((type . text) (text . What is 42 plus 58?)))))
 (tools
  ((name . add-numbers) (description . Add two numbers together)
   (input--schema (type . object)
    (properties (a (type . number)) (b (type . number))) (required a b)))))
Response:
The sum of 42 and 58 is 100

--- Testing Claude Tool Calling: get-current-time ---
$$ data:
((model . claude-sonnet-4-6) (max--tokens . 1000) (temperature . 0)
 (messages
  ((role . user) (content ((type . text) (text . What is the current time?)))))
 (tools
  ((name . get-current-time) (description . Get the current time)
   (input--schema (type . object)))))
Response:
Current time: 16:06:33 on 03/01/2026

--- Testing Claude Tool Calling: capitalize-text ---
$$ data:
((model . claude-sonnet-4-6) (max--tokens . 1000) (temperature . 0)
 (messages
  ((role . user)
   (content
    ((type . text) (text . Please capitalize the text 'hello world')))))
 (tools
  ((name . capitalize-text) (description . Convert text to uppercase)
   (input--schema (type . object) (properties (text (type . string)))
    (required text)))))
Response:
Capitalized: HELLO WORLD
t
* 
```

We didn't cover the implementation of the Gemini back end but here is example test code using the combined search and LLM capability:

```lisp
(format t "~%--- Testing Gemini Completions With Google Search ---~%")
(let ((response (gemini:generate-with-search "What sci-fi movies are playing in Flagstaff Arizona today?")))
  (format t "Response:~%~A~%" response))
```

The output (edited for brevity) looks like this:

```lisp
--- Testing Gemini Completions With Google Search ---
Response:
Today, Monday, March 2, 2026, there are several science fiction movies playing in Flagstaff, Arizona, primarily at the **Harkins Flagstaff 16** theater.

The following sci-fi films have confirmed showtimes for today:

### **Harkins Flagstaff 16**
*   **Avatar: Fire and Ash** (Sci-Fi/Adventure)
    *   **Showtime:** 12:55 PM (3D HFR)
*   **Bugonia** (Sci-Fi/Thriller/Comedy)
    *   **Showtime:** 5:30 PM

  ...
```



## Wrap Up

In this chapter we built a small but complete multi-provider LLM library in Common Lisp from the ground up. Starting with the shared llm.lisp utility layer, we established a clean foundation of curl-based HTTP communication and JSON manipulation that both the Claude and Ollama backends depend on without duplicating. The simple-tools.lisp package gave us a provider-agnostic tool registry built around the define-tool macro, making it possible to define a tool once and have it immediately available to any backend that knows how to read from *tools*.

The two provider backends demonstrated how different APIs can be wrapped behind a consistent completions interface despite having meaningfully different request and response shapes. Claude's input_schema key, typed content blocks, and stop_reason-based tool detection contrast with Ollama's OpenAI-compatible function schema and tool_calls response structure — yet from the caller's perspective both are invoked the same way, with the same tool names, and return the same kind of string result.

The example tools and test file showed the library working end to end: natural language queries being correctly routed to the right tool functions, arguments extracted from the model's JSON response and mapped to positional lambda parameters, and results returned as plain strings ready for further use or display.

A few things are worth keeping in mind as you build on this foundation. The single-pass tool dispatch is intentional in its simplicity but will need to grow into a proper agentic loop if you want the model to reason over tool results and decide on follow-up actions. The curl-based HTTP layer is easy to inspect and debug but would benefit from replacement with a native HTTP client in any long-running or high-throughput context.

