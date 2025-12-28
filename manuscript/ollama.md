# Local LLMs Using Llama

Running local models with Ollama offers several practical advantages for Common Lisp developers, especially those of us building exploratory or long-lived AI systems:

- Local inference eliminates network latency and external API dependencies, which simplifies debugging, improves reproducibility, and enables fully offline workflows—important when iterating on symbolic/LLM hybrids or REPL-driven experiments.
- Data never leaves the machine, providing strong privacy guarantees and avoiding compliance issues that can arise when sending prompts or intermediate representations to third-party services.
- Cost and rate-limit concerns disappear: once a model is downloaded, usage is bounded only by local compute, making it ideal for background agents, continuous evaluation, or batch reasoning tasks initiated from Lisp.
- Ollama’s simple HTTP interface fits naturally with Common Lisp’s strengths—process control, incremental development, and meta-programming, allowing developers to treat local language models as just another deterministic(ish) subsystem under their control.

The **ollama** package developed here provides generative AI code and tool use/function calling generative AI code in the directory **loving-common-lisp/src/ollama**.

## Design Notes (Optional Material)

Here we describe the design and architecture of the Ollama Common Lisp library, which provides an interface to the Ollama API for running local LLMs.

### 1. Common Utilities

The shared utilities are defined in `ollama-helper.lisp` and provide foundational functionality used by both basic completions and tool-calling APIs.

#### Configuration

- ***model-host*** — The Ollama API endpoint URL, defaulting to `http://localhost:11434/api/chat`

#### JSON Handling

- **lisp-to-json-string** — Converts Lisp data structures (alists) to JSON strings using `cl-json`
- **substitute-subseq** — String substitution utility used to work around `cl-json`'s encoding of `nil` as `null` (the Ollama API requires `false` for the stream parameter)

#### HTTP Communication

- **ollama-helper** — Core request handler that:
  1. Executes curl commands via `uiop:run-program`
  2. Parses JSON responses
  3. Extracts message content and tool calls from the response structure
  4. Returns multiple values: `(content function-calls)`

#### Package Definition

The `ollama` package (defined in `package.lisp`) exports:
- `completions`, `completions-with-tools` — Main API functions
- `summarize`, `answer-question` — Convenience wrappers
- `*model-name*`, `*tool-model-name*`, `*model-host*` — Configuration variables

### 2. Generative AI

Basic generative AI functionality is provided in `ollama.lisp` for simple text completions without tool calling.

#### Configuration

- ***model-name*** — Model identifier, defaults to `"mistral:v0.3"`

#### Core Functions

- **completions** — Sends a user prompt to the LLM and returns the text response
  - Constructs a message with role "user" and the provided content
  - Builds the request payload with model, stream (false), and messages
  - Uses `ollama-helper` to execute the request and extract content

#### Convenience Wrappers

- **summarize** — Prepends "Summarize: " to input text and calls `completions`
- **answer-question** — Formats input as a Q&A prompt and calls `completions`

#### Request Flow

```
User Text → Message Construction → JSON Encoding → curl Command → 
Ollama API → JSON Response → Content Extraction → Return String
```


### 3. Generative AI with Tools

Tool-calling (function calling) support is implemented in `ollama-tools.lisp`, enabling the LLM to invoke registered functions.

#### Configuration

- ***tool-model-name*** — Model for tool calling, defaults to `"mistral:v0.3"`
- ***available-functions*** — Hash table storing registered tool functions

#### Data Structures

- **ollama-function** — Struct containing:
  - `name` — Function identifier string
  - `description` — Human-readable description for the LLM
  - `parameters` — JSON Schema defining expected arguments
  - `handler` — Common Lisp function to invoke when called

#### Function Registration

- **register-tool-function** — Registers a tool with the system
  - Parameters: `name`, `description`, `parameters` (JSON Schema), `handler` (Lisp function)
  - Stores an `ollama-function` struct in `*available-functions*`

#### Tool Execution

- **handle-tool-function-call** — Processes an LLM tool call
  - Extracts function name and arguments from the response
  - Falls back to `infer-function-name-from-args` if model returns empty name
  - Looks up the registered handler and invokes it with the arguments
- **infer-function-name-from-args** — Workaround for models that return empty function names
  - Inspects argument keys to determine which function was intended

#### Main API

- **completions-with-tools** — Enhanced completion with tool support
  - Accepts prompt text and optional list of function names to enable
  - Builds tool definitions from registered functions
  - Sends request to Ollama with tools specification
  - Automatically invokes handlers when LLM returns tool calls

#### Built-in Tools

Two sample tools are pre-registered:

1. **get_weather** — Returns mock weather data for a location
   - Parameters: `location` (string) — The city name
   - Returns: Formatted weather string

2. **calculate** — Evaluates mathematical expressions
   - Parameters: `expression` (string) — Math expression like "2 + 2"
   - Uses Common Lisp's `eval` to compute results

#### Tool Call Flow

```
User Prompt + Tool Names → Build Tool Definitions → JSON Request →
Ollama API → Response with Tool Calls → Parse Function Call →
Lookup Handler → Invoke with Arguments → Return Result
```

#### Example Usage

```lisp
(ollama::completions-with-tools 
  "What's the weather like in New York?" 
  '("get_weather" "calculate"))
;; => "Weather in New York: Sunny, 72°F"
```


### System Definition

The ASDF system (`ollama.asd`) loads components in dependency order:

1. `package` — Package definition
2. `ollama-helper` — Shared utilities
3. `ollama-tools` — Tool-calling support
4. `ollama` — Basic completions

Dependencies: `uiop`, `cl-json`


## Implementation of Common Helper Code

The *defpackage* form for the **#:ollama** library establishes an isolated namespace for interacting with local Large Language Models. By inheriting functionality from #:cl, #:uiop, and #:cl-json, the package handles core logic, system-level file operations, and the JSON-heavy communication required by the Ollama REST API. The exported symbols define a public interface, ranging from high-level text processing functions like **summarize** and **answer-question**.

Listing of package.lisp:

```lisp
;;;; package.lisp

(defpackage #:ollama
  (:use #:cl #:uiop #:cl-json)
  (:export #:completions #:completions-with-tools
           #:summarize #:answer-question
           *model-name* *tool-model-name* *model-host*))
```

Listing of ollama.asd that defines a *defsystem* for this package:

```lisp
;;;; ollama.asd

(asdf:defsystem #:ollama
  :description "Library for using the ollama APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "ollama-helper")
               (:file "ollama-tools") 
               (:file "ollama")))
```

The following implementation establishes a bridge between Common Lisp and the Ollama local API, providing the infrastructure necessary for handling structured LLM interactions. By defining a dedicated **ollama** package and setting a default local Ollama server host variable, the code creates a controlled environment for external communication. The utility functions included here address two primary technical hurdles: the conversion of Lisp data structures into JSON-compliant strings for API consumption and a manual string substitution routine for fine-tuning command payloads. At the heart of this listing is a robust helper function that orchestrates a system-level **curl** call, capturing the resulting output and parsing the returned JSON. This process involves a traversal of the response object to isolate the model's textual content and any prospective tool calls, ensuring that the final output is returned in a format that Lisp can easily manipulate for downstream logic.

Listing of ollama-helper.lisp:

```lisp
(in-package #:ollama)

(defvar *model-host* "http://localhost:11434/api/chat")

(defun lisp-to-json-string (data)
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun ollama-helper (curl-command)
  (princ curl-command)
  (terpri)
  (handler-case
      (let ((response
             (uiop:run-program
              curl-command
              :output :string
              :error-output :string)))
        (princ "Raw response: ")
        (princ response)
        (terpri)
        (with-input-from-string
            (s response)
          (let* ((json-as-list (json:decode-json s))
                 (message (cdr (assoc :message json-as-list)))
                 (content (cdr (assoc :content message)))
                 ;; Extract function details from each tool_call
                 (function-calls (mapcar (lambda (tc)
                                           (cdr (assoc :function tc)))
                                         tool-calls)))
            (values content function-calls))))
    (error (e)
      (format t "Error executing curl command: ~a~%" e)
      nil)))
```

The code begins by setting up the environment with a global variable for the Ollama endpoint and helper functions for data transformation. The function **lisp-to-json-string** leverages the **cl-json** library to serialize data, while **substitute-subseq** provides a specialized way to replace substrings within the command strings. These utilities ensure that the data sent to the model is formatted correctly and that the commands remain flexible.

The core logic resides in **ollama-helper**, which uses **uiop:run-program** to execute a shell command and capture its output. The function is designed with error handling to manage potential connectivity or execution failures gracefully. Once a response is received, it decodes the JSON and performs an association list lookup to extract both the natural language message and any structured function calls, returning them as multiple values for the caller to process.

## Implementation of Generative AI Functionality

In this section, we examine a practical implementation of a Common Lisp client designed to interface with the Ollama local LLM inference service. The code defines a workflow for sending synchronous requests to a Large Language Model (LLM) by wrapping the system's **curl** utility to communicate with the Ollama API. By utilizing the **mistral:v0.3** model as a default, the program demonstrates how to structure Lisp data, specifically association lists, into the JSON format required by the endpoint. It includes a specific handling mechanism for boolean conversion, ensuring that Lisp's *nil* is correctly interpreted as a JSON false to disable streaming. Beyond the core transport logic, the listing provides high-level abstractions for common natural language processing tasks, such as summarization and question answering, illustrating how simple string concatenation can be used to format prompts that guide the model toward specific generative behaviors.

Listing of ollama.lisp:

```lisp
(in-package #:ollama)

;;; Basic Ollama completions without tool calling support
;;; For tool calling, see ollama-tools.lisp

(defvar *model-name* "mistral:v0.3")

(defun completions (starter-text)
  "Simple completion without function/tool calling support."
  (let* ((message (list (cons :|role| "user")
                        (cons :|content| starter-text)))
         (data (list (cons :|model| *model-name*)
                     (cons :|stream| nil)
                     (cons :|messages| (list message))))
         (json-data (lisp-to-json-string data))
         ;; Hack: cl-json encodes nil as null, but we need false for stream
         (fixed-json-data (substitute-subseq json-data ":null" ":false" :test #'string=))
         (curl-command
          (format nil "curl ~a -d ~s"
                  ollama::*model-host*
                  fixed-json-data)))
    (multiple-value-bind (content function-call)
        (ollama-helper curl-command)
      (declare (ignore function-call))
      (or content "No response content"))))

;;(ollama:completions "Complete the following text: The President went to")

;; Helper functions for summarization and question answering
(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))

(defun answer-question (some-text)
  (completions (concatenate 'string "
Q: " some-text "
A:")))
```

The core of this implementation lies in the **completions** function, which manages the transformation of Lisp structures into a command-line request. A notable detail is the manual string substitution used on the JSON payload; since many Common Lisp JSON libraries represent nil as null, the code explicitly replaces these occurrences with false to satisfy the Ollama API's requirement for the stream parameter. This ensures the function waits for a complete response rather than processing a continuous stream of tokens, simplifying the return value for the caller.

The program also showcases the extensibility of the base completion logic through the summarize and answer-question helper functions. These functions act as specialized wrappers that prepend task-specific instructions to the user input, effectively demonstrating "prompt engineering" within a programmatic context. By delegating the heavy lifting to the **ollama-helper** and the external **curl** command, the code remains focused on message preparation and providing a clean, functional interface for Lisp-based AI applications.

Sample output:

```lisp

```




## Implementation of Tool Use/Function Calling Generative AI Functionality


TBD

Listing of ollama-tools.lisp:

```lisp
(in-package #:ollama)

;;; Ollama completions with tool/function calling support
;;; Uses shared utilities from ollama-helper.lisp

(defvar *tool-model-name* "qwen3:1.7b")

(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct ollama-function
  name
  description
  parameters
  handler)  ;; Common Lisp function to handle the call

(defun register-tool-function (name description parameters handler)
  "Register a function that can be called by the LLM via tool calling.
   HANDLER is a Common Lisp function that takes a plist of arguments."
  (setf (gethash name *available-functions*)
        (make-ollama-function
         :name name
         :description description
         :parameters parameters
         :handler handler)))

(defun infer-function-name-from-args (args)
  "Infer the function name based on argument keys
   (workaround for models that return empty name)."
  (let ((arg-keys (mapcar #'car args)))
    (cond
      ((member :location arg-keys) "get_weather")
      ((member :expression arg-keys) "calculate")
      (t nil))))

(defun handle-tool-function-call (function-call)
  "Handle a function call returned from the LLM
   by invoking the registered handler."
  (format t "~%DEBUG handle-tool-function-call: ~a~%" function-call)
  (let* ((raw-name (cdr (assoc :name function-call)))
         (args (cdr (assoc :arguments function-call)))
         ;; If name is empty, try to infer from arguments
         (name (if (or (null raw-name) (string= raw-name ""))
                   (infer-function-name-from-args args)
                   raw-name))
         (func (gethash name *available-functions*)))
    (format t "DEBUG raw-name=~a inferred-name=~a args=~a func=~a~%"
            raw-name name args func)
    (if func
        (let ((handler (ollama-function-handler func)))
          (if handler
              (funcall handler args)
              (format nil
                      "No handler for function ~a, args: ~a" name args)))
        (error "Unknown function: ~a" name))))

(defun completions-with-tools (starter-text &optional functions)
  "Completion with function/tool calling support.
   STARTER-TEXT is the prompt to send to the LLM.
   FUNCTIONS is an optional list of registered function names
   to make available."
  (let* ((function-defs
           (when functions
             (mapcar
              (lambda (f)
                (let ((func (gethash f *available-functions*)))
                  (list
                   (cons :|name| (ollama-function-name func))
                   (cons :|description|
                         (ollama-function-description func))
                   (cons :|parameters|
                         (ollama-function-parameters func)))))
              functions)))
         (message (list (cons :|role| "user")
                        (cons :|content| starter-text)))
         (base-data (list (cons :|model| *tool-model-name*)
                          (cons :|stream| nil)
                          (cons :|messages| (list message))))
         (data (if function-defs
                   (append base-data
                           (list (cons :|tools| function-defs)))
                   base-data))
         (json-data (lisp-to-json-string data))
         ;; Hack: cl-json encodes nil as null, but we need false
         (fixed-json-data
           (substitute-subseq json-data ":null" ":false"
                              :test #'string=))
         (curl-command
           (format nil "curl ~a -d ~s"
                   ollama::*model-host*
                   fixed-json-data)))
    (multiple-value-bind (content function-call)
        (ollama-helper curl-command)
      (if function-call
          (handle-tool-function-call (car function-call))
          (or content "No response content")))))

;; Define handler functions

(defun get_weather (args)
  "Handler for get_weather tool. ARGS is an alist with :location key."
  (format t "get_weather called with args: ~a~%" args)
  (let ((location (cdr (assoc :location args))))
    (format nil "Weather in ~a: Sunny, 72°F" (or location "Unknown"))))

(defun calculate (args)
  "Handler for calculate tool. ARGS is an alist with :expression key."
  (let ((expression (cdr (assoc :expression args))))
    (if expression
        (handler-case
            (format nil "Result: ~a"
                    (eval (read-from-string expression)))
          (error (e) (format nil "Error calculating: ~a" e)))
        "No expression provided")))

;; Register sample functions with handlers
(register-tool-function
 "get_weather"
 "Get current weather for a location"
 (list (cons :|type| "object")
       (cons :|properties|
             (list (cons :|location|
                         (list (cons :|type| "string")
                               (cons :|description|
                                     "The city name")))))
       (cons :|required| '("location")))
 #'get_weather)

(register-tool-function
 "calculate"
 "Perform a mathematical calculation"
 (list (cons :|type| "object")
       (cons :|properties|
             (list (cons :|expression|
                         (list (cons :|type| "string")
                               (cons :|description|
                                     "Math expression like 2 + 2")))))
       (cons :|required| '("expression")))
 #'calculate)
```

Sample output (I include a lot of debug printout):

```lisp
* (ql:quickload :ollama)
To load "ollama":
  Load 1 ASDF system:
    ollama
; Loading "ollama"
[package ollama].

use:

(in-package :ollama)
nil
* (ollama::completions-with-tools "Use the get_weather tool for: What's the weather like in New York?" '("get_weather" "calculate"))
curl http://localhost:11434/api/chat -d "{\"model\":\"qwen3:1.7b\",\"stream\":false,\"messages\":[{\"role\":\"user\",\"content\":\"Use the get_weather tool for: What's the weather like in New York?\"}],\"tools\":[{\"name\":\"get_weather\",\"description\":\"Get current weather for a location\",\"parameters\":{\"type\":\"object\",\"properties\":{\"location\":{\"type\":\"string\",\"description\":\"The city name\"}},\"required\":[\"location\"]}},{\"name\":\"calculate\",\"description\":\"Perform a mathematical calculation\",\"parameters\":{\"type\":\"object\",\"properties\":{\"expression\":{\"type\":\"string\",\"description\":\"Math expression like 2 + 2\"}},\"required\":[\"expression\"]}}]}"
Raw response: {"model":"qwen3:1.7b","created_at":"2025-12-28T18:01:28.789187Z","message":{"role":"assistant","content":"","thinking":"Okay, the user is asking about the weather in New York. Let me check the available tools. There's a get_weather tool, which I think is meant to fetch weather information. The function name is probably \"get_weather\" and it takes a parameter, maybe the location. The user specified \"New York,\" so I need to call the get_weather function with \"New York\" as the argument. Let me make sure the parameters are correct. The tool's parameters are described as having a type \"properties\" but no specific details. Since the user provided the location, I'll pass that directly. I should structure the tool call with the name and arguments as a JSON object. Alright, that's it. Just call get_weather with \"New York\" as the argument.\n","tool_calls":[{"id":"call_fadz9if9","function":{"index":0,"name":"","arguments":{"location":"New York"}}}]},"done":true,"done_reason":"stop","total_duration":2964277542,"load_duration":79982833,"prompt_eval_count":150,"prompt_eval_duration":111485791,"eval_count":181,"eval_duration":2745854365}

DEBUG handle-tool-function-call: ((index . 0) (name . )
                                  (arguments (location . New York)))
DEBUG raw-name= inferred-name=get_weather args=((location . New York)) func=#S(ollama-function
                                                                               :name get_weather
                                                                               :description Get current weather for a location
                                                                               :parameters ((type
                                                                                             . object)
                                                                                            (properties
                                                                                             (location
                                                                                              (type
                                                                                               . string)
                                                                                              (description
                                                                                               . The city name)))
                                                                                            (required
                                                                                             location))
                                                                               :handler #<function ollama::get_weather>)
get_weather called with args: ((location . New York))
"Weather in New York: Sunny, 72°F"
* 
```


