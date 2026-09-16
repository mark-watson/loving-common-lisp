# Using Local LLMs With Ollama

Running local models with Ollama offers several practical advantages for Common Lisp developers, especially those of us building exploratory or long-lived AI systems:

- Local inference eliminates network latency and external API dependencies, which simplifies debugging, improves reproducibility, and enables fully offline workflows -- important when iterating on symbolic/LLM hybrids or REPL-driven experiments.
- Data never leaves the machine, providing strong privacy guarantees and avoiding compliance issues that can arise when sending prompts or intermediate representations to third-party services.
- Cost and rate-limit concerns disappear: once a model is downloaded, usage is bounded only by local compute, making it ideal for background agents, continuous evaluation, or batch reasoning tasks initiated from Lisp.
- Ollama's OpenAI-compatible HTTP interface fits naturally with Common Lisp's strengths -- process control, incremental development, and meta-programming, allowing developers to treat local language models as just another deterministic(ish) subsystem under their control.

The **ollama** package developed here provides generative AI code and tool use/function calling generative AI code in the directory **loving-common-lisp/src/ollama**. Under the hood, it leverages the **litelm** library (located in **loving-common-lisp/src/litelm**), which handles clean message serialization, OpenAI-compatible HTTP routing, streaming, and tool schemas using native Common Lisp data structures.

## Design Notes (Optional Material)

Here we describe the design and architecture of the Ollama Common Lisp library, which provides an interface to the Ollama API for running local LLMs using the **litelm** routing and transport engine.

![Architecture Diagram](images/ollama_architecture.jpg)

### 1. The litelm Foundation

Instead of invoking shell utilities like `curl` and manipulating raw JSON string substitutions, the `ollama` library builds on `litelm`. The `litelm` library provides:

- **Clean Lisp S-expression data structures**: Messages and tool specifications are written using native Common Lisp symbols, keywords, strings, and numbers rather than ad-hoc JSON dictionaries.
- **Unified model routing**: Models are addressed using a `"provider/model-name"` string prefix (e.g. `"ollama/qwen3-vl:2b"` for local Ollama or `"ollama-cloud/gpt-oss:120b-cloud"` for Ollama Cloud).
- **HTTP client management**: Built on `dexador`, requests are dispatched directly to OpenAI-compatible REST endpoints (`http://localhost:11434/v1` for local Ollama) without spawning external subprocesses.
- **Robust error handling**: HTTP errors map directly to a structured Common Lisp condition hierarchy (`litelm:api-error`, `litelm:rate-limit-error`, `litelm:authentication-error`, etc.).

### 2. Common Utilities and Configuration

The shared utilities are defined in `ollama-helper.lisp` and provide configuration used across basic completions, tool-calling, and cloud agent interfaces.

#### Configuration

- ***model-host*** -- The Ollama API endpoint base URL, defaulting to `http://localhost:11434/v1`

#### Routing Helper

- **ensure-model-name** -- Ensures that a model identifier string includes the `"ollama/"` provider prefix required by `litelm` when routing to the local server.

#### Package Definition

The `ollama` package (defined in `package.lisp`) exports:
- `completions`, `completions-with-tools` -- Main API functions
- `summarize`, `answer-question` -- Convenience wrappers
- `register-tool-function` -- Tool registry function
- `cloud-search-agent` -- Multi-turn autonomous agent for Ollama Cloud
- `*model-name*`, `*tool-model-name*`, `*model-host*`, `*cloud-model-name*`, `*cloud-host*` -- Configuration variables

### 3. Generative AI

Basic generative AI functionality is provided in `ollama.lisp` for text completions without tool calling.

#### Configuration

- ***model-name*** -- Model identifier, defaults to `"qwen3-vl:2b"`

#### Core Functions

- **completions** -- Sends a user prompt to the LLM and returns the text response string
  - Prepares the model name via `ensure-model-name`
  - Invokes `litelm:completion` with the prompt and `:api-base *model-host*`
  - Inspects the returned `litelm:response` struct and extracts content via `litelm:response-content`

#### Convenience Wrappers

- **summarize** -- Prepends "Summarize: " to input text and calls `completions`
- **answer-question** -- Formats input as a Q&A prompt and calls `completions`

#### Request Flow

```
User Text -> ensure-model-name -> litelm:completion -> HTTP POST ->
Ollama /v1/chat/completions -> litelm:response -> Extract Content -> Return String
```


### 4. Generative AI with Tools

Tool-calling (function calling) support is implemented in `ollama-tools.lisp`, enabling the LLM to invoke registered Common Lisp functions.

#### Configuration

- ***tool-model-name*** -- Model for tool calling, defaults to `"qwen3-vl:2b"`
- ***available-functions*** -- Hash table storing registered tool functions

#### Data Structures

- **ollama-function** -- Struct containing:
  - `name` -- Function identifier string
  - `description` -- Human-readable description for the LLM
  - `parameters` -- Parameter specification in litelm format
  - `handler` -- Common Lisp function to invoke when called

#### Function Registration

- **register-tool-function** -- Registers a tool with the system
  - Parameters: `name`, `description`, `parameters`, `handler` (Lisp function)
  - Accepts parameters directly as clean Lisp lists: `((param-name type desc &key required enum) ...)`
  - Stores an `ollama-function` struct in `*available-functions*`

#### Tool Execution

- **handle-tool-function-call** -- Processes an LLM tool call
  - Extracts function name and arguments from the `litelm:response-tool-calls` plist
  - Falls back to `infer-function-name-from-args` if the model returns an empty name
  - Looks up the registered handler and invokes it with the decoded argument alist
- **infer-function-name-from-args** -- Workaround for models that return empty function names
  - Inspects argument keys to determine which function was intended

#### Main API

- **completions-with-tools** -- Enhanced completion with tool support
  - Accepts prompt text and an optional list of function name strings to enable
  - Translates registered functions into `litelm` tool definitions
  - Sends request to Ollama via `litelm:completion`
  - Automatically dispatches handlers when the model returns tool calls

#### Built-in Tools

Two sample tools are pre-registered:

1. **get_weather** -- Returns mock weather data for a location
   - Parameters: `location` (string) -- The city name
   - Returns: Formatted weather string

2. **calculate** -- Evaluates mathematical expressions
   - Parameters: `expression` (string) -- Math expression like "2 + 2"
   - Uses Common Lisp's `eval` to compute results

#### Tool Call Flow

```
User Prompt + Tool Names -> Build Tool Definitions -> litelm:completion ->
Ollama API -> Response with Tool Calls -> Parse Function Call ->
Lookup Handler -> Invoke with Arguments -> Return Result
```

#### Example Usage

```lisp
(ollama:completions-with-tools 
  "Use the get_weather tool for: What's the weather like in New York?" 
  '("get_weather" "calculate"))
;; => "Weather in New York: Sunny, 72°F"
```


### 5. Multi-Turn Autonomous Agent with Ollama Cloud

The file `ollama-cloud-search.lisp` demonstrates how to integrate Common Lisp with the Ollama Cloud service to create an autonomous agent that searches the web and fetches web pages to answer current questions.

- Defines explicit tool schemas for `web_search` and `web_fetch` in litelm's declarative tool format.
- Registers the `:ollama-cloud` provider with `litelm:define-provider`, binding it to `"https://ollama.com/v1"`.
- Runs an iterative agent loop that maintains conversation history with `litelm` message lists:
  - Appends user prompts: `(:user prompt)`
  - Records model responses and tool requests: `(:assistant content :tool-calls tool-calls)`
  - Executes requested local functions (e.g. querying DuckDuckGo or fetching URLs)
  - Feeds results back into the conversation: `(:tool result :tool-call-id id)`
  - Loops until the model determines it has enough information to synthesize a final answer.


### System Definition

The ASDF system (`ollama.asd`) loads components in dependency order:

1. `package` -- Package definition
2. `ollama-helper` -- Shared utilities and routing configuration
3. `ollama-tools` -- Tool-calling support and tool registry
4. `ollama` -- Basic completions and convenience wrappers
5. `ollama-cloud-search` -- Ollama Cloud autonomous search agent

Dependencies: `litelm`, `uiop`


## Implementation of Common Helper Code

The *defpackage* form for the **#:ollama** library establishes an isolated namespace for interacting with local Large Language Models. By relying on `#:cl`, the package provides a clean public interface for high-level text processing functions like **summarize** and **answer-question**, as well as tool-calling routines and cloud agents.

Listing of package.lisp:

```lisp
;;;; package.lisp

(defpackage #:ollama
  (:use #:cl)
  (:export #:completions
           #:completions-with-tools
           #:summarize
           #:answer-question
           #:register-tool-function
           #:cloud-search-agent
           #:*model-name*
           #:*tool-model-name*
           #:*model-host*
           #:*cloud-model-name*
           #:*cloud-host*))
```

Listing of ollama.asd that defines a *defsystem* for this package:

```lisp
;;;; ollama.asd

(asdf:defsystem #:ollama
  :description "Library for using the ollama APIs via litelm"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:litelm #:uiop)
  :components ((:file "package")
               (:file "ollama-helper")
               (:file "ollama-tools") 
               (:file "ollama")
               (:file "ollama-cloud-search")))
```

The following helper code sets up the environment with a default endpoint and model routing utility. In contrast to earlier designs that performed manual JSON encoding and spawned shell processes, `ollama-helper.lisp` simply manages the local host endpoint configuration and ensures that model identifiers include the appropriate routing prefix for `litelm`.

Listing of ollama-helper.lisp:

```lisp
(in-package #:ollama)

(defvar *model-host* "http://localhost:11434/v1")

(defun ensure-model-name (model)
  "Ensure MODEL has a provider prefix for litelm routing (defaults to ollama/)."
  (if (find #\/ model)
      model
      (concatenate 'string "ollama/" model)))
```


## Implementation of Generative AI Functionality

In this section, we examine the implementation of basic synchronous text completions. The `completions` function routes requests through `litelm:completion`, which handles HTTP communication to the local Ollama server. Beyond the core completion call, the listing provides high-level abstractions for common natural language processing tasks, such as summarization and question answering, illustrating how simple string concatenation formats prompts that guide the model toward specific generative behaviors.

Listing of ollama.lisp:

```lisp
(in-package #:ollama)

;;; Basic Ollama completions without tool calling support
;;; For tool calling, see ollama-tools.lisp

(defvar *model-name* "qwen3-vl:2b")

(defun completions (starter-text &key (model *model-name*))
  "Simple completion without function/tool calling support."
  (let* ((full-model (ensure-model-name model))
         (resp (litelm:completion full-model
                                  :messages starter-text
                                  :api-base *model-host*)))
    (format t "Raw response: ~s~%" (litelm:response-raw resp))
    (or (litelm:response-content resp) "No response content")))

;;(ollama:completions "Complete the following text: The President went to")

;; Helper functions for summarization and question answering
(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))

(defun answer-question (some-text)
  (completions (concatenate 'string "
Q: " some-text "
A:")))
```

The core of this implementation lies in the **completions** function. By delegating transport, HTTP connections, and JSON parsing to **litelm**, the code remains concise and free of low-level shell calls. The raw response is printed for debugging transparency, and the textual answer is extracted directly with `litelm:response-content`.

The program also showcases the extensibility of the base completion logic through the `summarize` and `answer-question` helper functions. These functions act as specialized wrappers that prepend task-specific instructions to the user input, effectively demonstrating basic prompt engineering within a programmatic Common Lisp context.


## Implementation of Tool Use/Function Calling Generative AI Functionality

The following listing implements tool-calling (also known as function-calling) within a Common Lisp environment using Ollama and `litelm`. By defining a custom **ollama-function** structure and a global registry via a hash table, the code allows developers to map Large Language Model (LLM) tool requests directly to native Lisp handlers. The primary entry point, **completions-with-tools**, translates the registered tools into `litelm`'s native Lisp tool format and submits them to Ollama. When the model requests a tool call, the dispatcher unpacks the arguments and invokes the corresponding Common Lisp function.

Listing of ollama-tools.lisp:

```lisp
(in-package #:ollama)

;;; Ollama completions with tool/function calling support
;;; Uses litelm for model routing, message handling, and tool schemas.

(defvar *tool-model-name* "qwen3-vl:2b")

(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct ollama-function
  name
  description
  parameters
  handler)  ;; Common Lisp function to handle the call

(defun register-tool-function (name description parameters handler)
  "Register a function that can be called by the LLM via tool calling.
   NAME is a string or symbol.
   PARAMETERS is a list of parameters in litelm format:
     ((param-name param-type param-description &key required enum) ...)
   HANDLER is a Common Lisp function that takes an alist of arguments."
  (let ((tool-name (string-downcase (string name))))
    (setf (gethash tool-name *available-functions*)
          (make-ollama-function
           :name tool-name
           :description description
           :parameters parameters
           :handler handler))))

(defun infer-function-name-from-args (args)
  "Infer the function name based on argument keys
   (workaround for models that return empty name)."
  (let ((arg-keys (mapcar (lambda (pair)
                            (string-downcase (string (car pair))))
                          args)))
    (cond
      ((member "location" arg-keys :test #'string=) "get_weather")
      ((member "expression" arg-keys :test #'string=) "calculate")
      (t nil))))

(defun handle-tool-function-call (function-call)
  "Handle a function call returned from the LLM
   by invoking the registered handler."
  (format t "~%DEBUG handle-tool-function-call: ~a~%" function-call)
  (let* ((raw-name (or (getf function-call :name)
                       (cdr (assoc :name function-call))))
         (args (or (getf function-call :arguments)
                   (cdr (assoc :arguments function-call))))
         ;; If name is empty, try to infer from arguments
         (name (if (or (null raw-name) (string= raw-name ""))
                   (infer-function-name-from-args args)
                   raw-name))
         (func (gethash (string-downcase (string name)) *available-functions*)))
    (format t "DEBUG raw-name=~a inferred-name=~a args=~a func=~a~%"
            raw-name name args func)
    (if func
        (let ((handler (ollama-function-handler func)))
          (if handler
              (funcall handler args)
              (format nil
                      "No handler for function ~a, args: ~a" name args)))
        (error "Unknown function: ~a" name))))

(defun %convert-to-litelm-tools (functions)
  "Convert registered tool names into litelm tool definitions:
   ((name description ((param-name param-type param-desc ...) ...)) ...)"
  (mapcar (lambda (f)
            (let* ((name-str (string-downcase (string f)))
                   (func (gethash name-str *available-functions*)))
              (unless func
                (error "Unknown tool function: ~a" f))
              (list (ollama-function-name func)
                    (ollama-function-description func)
                    (ollama-function-parameters func))))
          functions))

(defun completions-with-tools (starter-text &optional functions)
  "Completion with function/tool calling support.
   STARTER-TEXT is the prompt to send to the LLM.
   FUNCTIONS is an optional list of registered function names to make available."
  (let* ((full-model (ensure-model-name *tool-model-name*))
         (tool-defs (when functions
                      (%convert-to-litelm-tools functions)))
         (resp (litelm:completion full-model
                                  :messages starter-text
                                  :tools tool-defs
                                  :api-base *model-host*)))
    (format t "Raw response: ~s~%" (litelm:response-raw resp))
    (let ((tool-calls (litelm:response-tool-calls resp)))
      (if tool-calls
          (handle-tool-function-call (first tool-calls))
          (or (litelm:response-content resp) "No response content")))))

;; Define handler functions

(defun get_weather (args)
  "Handler for get_weather tool. ARGS is an alist with :location key."
  (format t "get_weather called with args: ~a~%" args)
  (let ((location (or (cdr (assoc :location args :test #'string-equal))
                      (cdr (assoc "location" args :test #'string-equal)))))
    (format nil "Weather in ~a: Sunny, 72°F" (or location "Unknown"))))

(defun calculate (args)
  "Handler for calculate tool. ARGS is an alist with :expression key."
  (format t "calculate called with args: ~a~%" args)
  (let ((expression (or (cdr (assoc :expression args :test #'string-equal))
                        (cdr (assoc "expression" args :test #'string-equal)))))
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
 '((location "string" "The city name"))
 #'get_weather)

(register-tool-function
 "calculate"
 "Perform a mathematical calculation"
 '((expression "string" "Math expression like 2 + 2"))
 #'calculate)
```

The core of this system lies in decoupling tool definitions from their execution logic. By using the **register-tool-function** routine, you can define tools using clean Common Lisp parameter lists, specifying parameter names, types, and descriptions, while simultaneously binding them to a specific Lisp function. This allows the **handle-tool-function-call** dispatcher to act as a bridge, looking up the appropriate handler in the `*available-functions*` hash table and executing it with the arguments returned by the LLM.

Additionally, the calculate tool demonstrates the dynamism of Common Lisp by using `read-from-string` and `eval`, allowing the LLM to execute mathematical expressions directly within the Lisp runtime.

Sample REPL session:

```lisp
* (ql:quickload :ollama)
To load "ollama":
  Load 1 ASDF system:
    ollama
; Loading "ollama"
[package ollama].

* (ollama:completions-with-tools "Use the get_weather tool for: What's the weather like in New York?" '("get_weather" "calculate"))
Raw response: (("id" . "chatcmpl-5") ("object" . "chat.completion")
               ("created" . 1789572213) ("model" . "qwen3-vl:2b")
               ("system_fingerprint" . "fp_ollama")
               ("choices"
                (("index" . 0)
                 ("message" ("role" . "assistant") ("content" . "")
                  ("reasoning"
                   . "The user is asking for the weather in New York. I need to use the get_weather tool.")
                  ("tool_calls"
                   (("id" . "call_4h6756rc") ("index" . 0)
                    ("type" . "function")
                    ("function" ("name" . "get_weather")
                     ("arguments" . "{\"location\":\"New York\"}")))))
                 ("finish_reason" . "tool_calls")))
               ("usage" ("prompt_tokens" . 238) ("completion_tokens" . 132) ("total_tokens" . 370)))

DEBUG handle-tool-function-call: (ID call_4h6756rc NAME get_weather ARGUMENTS ((LOCATION . New York)))
DEBUG raw-name=get_weather inferred-name=get_weather args=((LOCATION . New York)) func=#S(OLLAMA-FUNCTION :NAME get_weather ...)
get_weather called with args: ((LOCATION . New York))
"Weather in New York: Sunny, 72°F"
```


## Using Built In Web Search Tool on Ollama Cloud

The file **ollama-cloud-search.lisp** demonstrates how to integrate Common Lisp with the Ollama Cloud API to create an autonomous agent capable of performing real-time web searches and content retrieval. By defining explicit tool specifications for `web_search` and `web_fetch`, this example code instructs a hosted model running on Ollama Cloud to identify when it requires external data to fulfill a user request.

By using `litelm`, tool specifications and messages are represented as standard Lisp lists. The agent loop iteratively calls `litelm:completion`, executes any requested tool calls, appends the tool results to the conversation history, and continues until the model returns a synthesized natural-language answer.

Listing of ollama-cloud-search.lisp:

```lisp
(in-package #:ollama)

;;; Ollama Cloud agent with web_search and web_fetch tool calling using litelm.
;;; Requires OLLAMA_API_KEY to be set in the environment.

(defvar *cloud-model-name* "gpt-oss:120b-cloud")
(defvar *cloud-host* "https://ollama.com/v1")

;; Register Ollama Cloud provider with litelm
(eval-when (:load-toplevel :execute)
  (litelm:define-provider :ollama-cloud "https://ollama.com/v1"
    :env-keys '("OLLAMA_API_KEY")))

;;; Tool definitions in litelm format

(defvar *cloud-search-tools*
  '((web_search "Search the web for current information"
      ((query "string" "The search query string")))
    (web_fetch "Fetch the content of a web page by URL"
      ((url "string" "The URL to fetch")))))

;;; API key helper

(defun get-api-key ()
  "Read OLLAMA_API_KEY from the environment. Signals an error if not set."
  (or (uiop:getenv "OLLAMA_API_KEY")
      (error "OLLAMA_API_KEY environment variable is not set")))

;;; Tool execution

(defun execute-web-search (args)
  "Search the web via DuckDuckGo. ARGS is an alist with :query key."
  (let* ((query (or (cdr (assoc :query args :test #'string-equal))
                    (cdr (assoc "query" args :test #'string-equal))
                    ""))
         (encoded (substitute #\+ #\Space query))
         (url (format nil
                      "https://api.duckduckgo.com/?q=~a&format=json&no_html=1&skip_disambig=1"
                      encoded))
         (curl-cmd (format nil "curl -s --max-time 10 ~s" url)))
    (format t "  [web_search] query: ~a~%" query)
    (handler-case
        (let ((result (uiop:run-program curl-cmd :output :string :error-output :string)))
          (format t "  [web_search] got ~a chars~%" (length result))
          result)
      (error (e) (format nil "web_search error: ~a" e)))))

(defun execute-web-fetch (args)
  "Fetch the content of a URL. ARGS is an alist with :url key."
  (let* ((url (or (cdr (assoc :url args :test #'string-equal))
                  (cdr (assoc "url" args :test #'string-equal))
                  ""))
         (curl-cmd (format nil "curl -s -L --max-time 15 ~s" url)))
    (format t "  [web_fetch] url: ~a~%" url)
    (handler-case
        (let ((result (uiop:run-program curl-cmd :output :string :error-output :string)))
          (format t "  [web_fetch] got ~a chars~%" (length result))
          ;; Limit size to avoid overwhelming the model context
          (subseq result 0 (min 4000 (length result))))
      (error (e) (format nil "web_fetch error: ~a" e)))))

;;; Agent loop using litelm

(defun cloud-search-agent (prompt &key (model *cloud-model-name*))
  "Agent loop: calls Ollama Cloud with web_search and web_fetch tools,
   executing any tool calls and feeding results back until the model
   returns a final answer. Returns the final answer string."
  (let ((messages (list (list :user prompt)))
        (full-model (if (find #\/ model)
                        model
                        (concatenate 'string "ollama-cloud/" model))))
    (loop
      (format t "~%Calling Ollama Cloud (~a)...~%" model)
      (let* ((resp (litelm:completion full-model
                                      :messages messages
                                      :tools *cloud-search-tools*
                                      :api-base *cloud-host*
                                      :api-key (get-api-key)))
             (content (litelm:response-content resp))
             (tool-calls (litelm:response-tool-calls resp)))
        (format t "Raw response: ~s~%" (litelm:response-raw resp))
        (cond
          ;; Model requested one or more tool calls
          (tool-calls
           (format t "~%Model requested ~a tool call(s).~%" (length tool-calls))
           ;; Append assistant message with tool calls to history
           (setf messages
                 (append messages
                         (list (list :assistant content :tool-calls tool-calls))))
           (dolist (tc tool-calls)
             (let* ((name (getf tc :name))
                    (args (getf tc :arguments))
                    (result
                      (cond
                        ((string-equal name "web_search") (execute-web-search args))
                        ((string-equal name "web_fetch")  (execute-web-fetch args))
                        (t (format nil "Unknown tool: ~a" name)))))
               (format t "  Tool ~a completed.~%" name)
               ;; Append tool result to history in litelm message format
               (setf messages
                     (append messages
                             (list (list :tool (format nil "~a" result)
                                         :tool-call-id (getf tc :id))))))))
          ;; No tool calls - this is the final answer
          (t
           (format t "~%Final Answer: ~a~%" content)
           (return (or content "No response"))))))))
```

The core of the implementation lies in the `cloud-search-agent` loop, which manages the stateful conversation history between the user and the assistant. When the model determines that a query requires current information (such as the price of a cryptocurrency or recent corporate news), it returns tool calls instead of a final text response. The Lisp code parses these calls, dispatches the appropriate local functions (`execute-web-search` or `execute-web-fetch`), and appends the results to the message list with the `:tool` role. This enables the model to see the results of its requested actions in the subsequent turn.

Additionally, the `execute-web-fetch` function enforces a character limit on the returned HTML/text content to prevent overwhelming the model's context window.

Here is an example search tool session:

```
$ sbcl
* (ql:quickload :ollama)
To load "ollama":
  Load 1 ASDF system:
    ollama
; Loading "ollama"
* (ollama:cloud-search-agent "What is the current price of Bitcoin?")

Calling Ollama Cloud (gpt-oss:120b-cloud)...
Raw response: ...
Model requested 1 tool call(s).
  [web_search] query: current price of Bitcoin USD
  [web_search] got 1252 chars
  Tool web_search completed.

Calling Ollama Cloud (gpt-oss:120b-cloud)...
Raw response: ...
Model requested 1 tool call(s).
  [web_fetch] url: https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&vs_currencies=usd
  [web_fetch] got 25 chars
  Tool web_fetch completed.

Calling Ollama Cloud (gpt-oss:120b-cloud)...
Raw response: ...

Final Answer: **Current Bitcoin Price (USD)**: approx. **$71,560**  

*Source:* CoinGecko API (simple price endpoint)
```


## Ollama Chapter Wrap Up

Dear reader, this chapter demonstrates that the marriage of Common Lisp's symbolic strengths and Ollama's local inference creates a powerful environment for building autonomous, privacy-respecting AI systems. By utilizing the **litelm** routing engine, we have eliminated brittle subprocess calls and string replacement hacks, replacing them with idiomatic Lisp S-expressions for messages, tools, and response handling.

The architecture we developed -- centered around a central dispatcher, a declarative tool registry, and an autonomous agent loop -- allows the LLM to behave as a high-level controller that can orchestrate native Lisp code to perform calculations, fetch weather data, or query the live web.

Looking ahead, the shift from single-turn local execution to multi-turn cloud and local agents illustrates the evolving landscape of AI development. Whether you are leveraging the low latency and zero cost of a local Qwen model instance or the broad capabilities of a cloud-based search agent, the patterns established here provide a solid foundation for any modern Lisp-based AI application.


## Optional Practice Problems

1. **Custom Temperature and Parameter Configuration**:
   Extend the `completions` function in [ollama.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama.lisp) to support optional configuration parameters such as `:temperature`, `:max-tokens`, or `:top-p`. Pass these parameters directly to `litelm:completion` via keyword arguments and verify that the local model adjusts its creativity or output length accordingly.

2. **System Prompt Support**:
   Modify the message construction inside `completions` in [ollama.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama.lisp) to accept a `:system-prompt` keyword argument. If provided, format the messages list as `((:system system-prompt) (:user starter-text))` when invoking `litelm:completion`. Test this extension by instructing the local LLM to restrict its answers to a specific persona or output format.

3. **Streaming Responses to the REPL**:
   The `litelm` library supports streaming via the `:stream` keyword argument to `litelm:completion`. Modify `completions` in [ollama.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama.lisp) to accept an optional `:stream` argument (defaulting to `nil`). When `t`, pass a callback to `litelm:completion` that immediately writes each token delta to `*standard-output*` and flushes with `finish-output`.

4. **Multi-Step Local Tool Execution Loop**:
   Currently, `completions-with-tools` in [ollama-tools.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama-tools.lisp) executes a single tool call and returns the result string. Extend `completions-with-tools` to run an agent loop similar to `cloud-search-agent` in [ollama-cloud-search.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama-cloud-search.lisp). Recursively feed the tool execution results back to the local model until it stops requesting tools and provides a final natural-language response.

5. **Tool Error Handling and Recovery**:
   When a tool handler triggers an error (such as a division by zero in `calculate`), wrap the handler call in `handler-case` and return a descriptive error message as the tool result string. Pass this result back to the model in the multi-turn loop, observing how the LLM attempts to correct its mistake or inform the user.

6. **Web Search Fallback for Local Models**:
   Combine the tool-calling mechanism of `completions-with-tools` from [ollama-tools.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama-tools.lisp) and the DuckDuckGo query mechanism of `execute-web-search`/`execute-web-fetch` from [ollama-cloud-search.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama-cloud-search.lisp) to run entirely on a local model (such as `qwen3-vl:2b` or `qwen3.5:2b`). Register the web tools locally and test how effectively small local models can perform multi-turn search queries.
