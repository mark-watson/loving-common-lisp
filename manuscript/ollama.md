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
  - Runs the full `litelm` tool-use loop: dispatches *every* tool call the model
    returns, appends each result as a `:tool` message, and calls the model again
    until it stops requesting tools and answers in natural language

#### Built-in Tools

Two sample tools are pre-registered:

1. **get_weather** -- Returns mock weather data for a location
   - Parameters: `location` (string) -- The city name
   - Returns: Formatted weather string

2. **calculate** -- Evaluates mathematical expressions
   - Parameters: `expression` (string) -- Infix math expression like "2 + 2"
   - Tokenizes and parses the infix text into a prefix Lisp form, then computes
     it with `%evaluate-math`, which handles only numbers and the operators
     `+ - * /` -- no `eval` and no symbol resolution, so LLM-supplied text
     cannot execute arbitrary Lisp

#### Tool Call Flow

```
User Prompt + Tool Names -> Build Tool Definitions -> litelm:completion ->
Ollama API -> Response with Tool Calls -> Parse Function Calls ->
For Each Call: Lookup Handler -> Invoke with Arguments -> Append :tool Result ->
litelm:completion (repeat until no tool calls) -> Final Answer
```

#### Example Usage

```lisp
(ollama:completions-with-tools 
  "Use the get_weather tool for: What's the weather like in New York?" 
  '("get_weather" "calculate"))
;; => "The weather in New York is currently Sunny with a temperature of 72°F."
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

The following listing implements tool-calling (also known as function-calling) within a Common Lisp environment using Ollama and `litelm`. By defining a custom **ollama-function** structure and a global registry via a hash table, the code allows developers to map Large Language Model (LLM) tool requests directly to native Lisp handlers. The primary entry point, **completions-with-tools**, translates the registered tools into `litelm`'s native Lisp tool format and submits them to Ollama. When the model requests tool calls, the dispatcher unpacks the arguments and invokes the corresponding Common Lisp functions. Because a single model turn may request several tools — and because the model has not yet seen any tool output when it makes that request — **completions-with-tools** runs the complete `litelm` tool-use loop: it executes *every* requested call, appends each result to the conversation as a `:tool` message, and calls the model again, repeating until the model stops asking for tools and returns a synthesized final answer.

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
   FUNCTIONS is an optional list of registered function names to make available.
   Runs the complete litelm tool-use loop: every tool call the model requests is
   dispatched, each result is appended to the conversation as a :tool message,
   and the model is called again until it returns a plain final answer.
   Returns the final answer string."
  (let* ((full-model (ensure-model-name *tool-model-name*))
         (tool-defs (when functions
                      (%convert-to-litelm-tools functions)))
         (messages (list (list :user starter-text))))
    (loop
      (let* ((resp (litelm:completion full-model
                                      :messages messages
                                      :tools tool-defs
                                      :api-base *model-host*))
             (content (litelm:response-content resp))
             (tool-calls (litelm:response-tool-calls resp)))
        (format t "Raw response: ~s~%" (litelm:response-raw resp))
        (if tool-calls
            (progn
              (format t "~%Model requested ~a tool call(s).~%" (length tool-calls))
              ;; Record the assistant turn that asked for the tool calls.
              (setf messages
                    (append messages
                            (list (list :assistant content :tool-calls tool-calls))))
              ;; Execute every requested call and feed each result back.
              (dolist (tc tool-calls)
                (let ((result (handle-tool-function-call tc)))
                  (format t "  Tool ~a completed.~%" (getf tc :name))
                  (setf messages
                        (append messages
                                (list (list :tool (format nil "~a" result)
                                            :tool-call-id (getf tc :id))))))))
            ;; No tool calls - the model produced its final answer.
            (return (or content "No response content")))))))

;; Define handler functions

(defun get_weather (args)
  "Handler for get_weather tool. ARGS is an alist with :location key."
  (format t "get_weather called with args: ~a~%" args)
  (let ((location (or (cdr (assoc :location args :test #'string-equal))
                      (cdr (assoc "location" args :test #'string-equal)))))
    (format nil "Weather in ~a: Sunny, 72°F" (or location "Unknown"))))

;;; The calculate tool evaluates ordinary infix arithmetic ("2 + 2",
;;; "(3 + 4) * 2") written by the LLM. That text is untrusted, so the path from
;;; string to number is deliberately narrow:
;;;   1. Tokens are restricted to ASCII digits, "." and the operators + - * / ( ).
;;;   2. Number literals are read with *READ-EVAL* bound to NIL, which disables
;;;      #. read-time evaluation.
;;;   3. The parsed form is walked by %EVALUATE-MATH, which understands only
;;;      numbers and the four operators. No symbol is ever resolved and EVAL is
;;;      never called, so LLM-supplied text cannot execute arbitrary Lisp.
;;; Expression length is capped so a hostile or runaway prompt cannot consume
;;; unbounded time or memory building huge bignums and rationals.

(defvar *calculate-max-expression-length* 1000
  "Maximum number of characters accepted by the calculate tool's :expression.")

(defun %ascii-digit-char-p (ch)
  "True if CH is one of the ASCII digits 0-9."
  (char<= #\0 ch #\9))

(defun %tokenize-math (string)
  "Split the infix math STRING into a vector of numbers and operator characters."
  (let ((*read-eval* nil)
        (tokens '())
        (number (make-string-output-stream)))
    (labels ((flush-number ()
               (let ((text (get-output-stream-string number)))
                 (when (plusp (length text))
                   (push (read-from-string text) tokens)))))
      (loop for ch across string do
        (cond ((or (%ascii-digit-char-p ch) (char= ch #\.))
               (write-char ch number))
              ((find ch "+-*/()") (flush-number) (push ch tokens))
              (t (flush-number))))
      (flush-number))
    (coerce (nreverse tokens) 'vector)))

(defun %parse-math (tokens)
  "Parse the infix TOKENS into a prefix Lisp form. Grammar:
     expression := term (('+' | '-') term)*
     term       := factor (('*' | '/') factor)*
     factor     := number | '(' expression ')' | '-' factor"
  (let ((pos 0))
    (labels ((peek () (when (< pos (length tokens)) (aref tokens pos)))
             (take () (prog1 (peek) (incf pos)))
             (expression ()
               (let ((left (term)))
                 (loop for op = (peek)
                       while (member op '(#\+ #\-))
                       do (take)
                          (setf left (list (if (char= op #\+) '+ '-)
                                           left (term)))
                       finally (return left))))
             (term ()
               (let ((left (factor)))
                 (loop for op = (peek)
                       while (member op '(#\* #\/))
                       do (take)
                          (setf left (list (if (char= op #\*) '* '/)
                                           left (factor)))
                       finally (return left))))
             (factor ()
               (let ((token (take)))
                 (cond ((null token) (error "Unexpected end of expression"))
                       ((numberp token) token)
                       ((char= token #\() (prog1 (expression)
                                            (unless (eql (take) #\))
                                              (error "Missing close parenthesis"))))
                       ((char= token #\-) (list '- (factor)))
                       (t (error "Unexpected token ~s" token))))))
      (let ((form (expression)))
        (when (peek)
          (error "Unexpected trailing input"))
        form))))

(defun %evaluate-math (form)
  "Evaluate a parsed arithmetic FORM of numbers and the operators + - * /.
Returns a number. Signals an error for any other shape, so a malformed tree can
never reach the Lisp evaluator."
  (cond
    ((numberp form) form)
    ;; Unary minus, produced by FACTOR for input such as "-3 + 10".
    ((and (consp form) (eq (car form) '-) (null (cddr form)))
     (- (%evaluate-math (second form))))
    ;; Binary operation: exactly three elements, operator restricted to + - * /.
    ((and (consp form) (member (car form) '(+ - * /)) (null (cdddr form)))
     (let ((left (%evaluate-math (second form)))
           (right (%evaluate-math (third form))))
       (ecase (car form)
         (+ (+ left right))
         (- (- left right))
         (* (* left right))
         (/ (/ left right)))))
    (t (error "Refusing to evaluate unsafe expression form: ~s" form))))

(defun calculate (args)
  "Handler for calculate tool. ARGS is an alist with :expression key."
  (format t "calculate called with args: ~a~%" args)
  (let ((expression (or (cdr (assoc :expression args :test #'string-equal))
                        (cdr (assoc "expression" args :test #'string-equal)))))
    (if expression
        (handler-case
            (progn
              (when (> (length expression) *calculate-max-expression-length*)
                (error "Expression too long (~a characters, limit is ~a)"
                       (length expression) *calculate-max-expression-length*))
              (format nil "Result: ~a"
                      (%evaluate-math (%parse-math (%tokenize-math expression)))))
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

The core of this system lies in decoupling tool definitions from their execution logic. By using the **register-tool-function** routine, you can define tools using clean Common Lisp parameter lists, specifying parameter names, types, and descriptions, while simultaneously binding them to a specific Lisp function. This allows the **handle-tool-function-call** dispatcher to act as a bridge, looking up the appropriate handler in the `*available-functions*` hash table and executing it with the arguments returned by the LLM. The surrounding loop in **completions-with-tools** then closes the circuit: each handler result is added back to `messages` as a `:tool` message carrying the originating call's `:tool-call-id`, so the next `litelm:completion` call lets the model read the tool output and answer the user in natural language.

Additionally, the calculate tool shows what it takes to run a *safe* evaluator over untrusted model output. Because the model writes human-style infix arithmetic, `%tokenize-math` splits the text into numbers and operator characters and `%parse-math` turns that token stream into a prefix Lisp form -- but the result is never handed to `eval`. Instead `%evaluate-math` walks the form itself, accepting only numbers and the operators `+ - * /`, so no symbol from the LLM is ever resolved and no arbitrary code can run. Three further details matter: the tokenizer accepts only ASCII digits (not `digit-char-p`, which on SBCL also returns true for non-ASCII digits such as Arabic-Indic `U+0663`), number literals are read with `*read-eval*` bound to `nil` so `#.` read-time evaluation is disabled, and the expression length is capped so a runaway prompt cannot build enormous bignums or rationals. A naive `(eval (read-from-string expression))` would be wrong twice over: `read-from-string` stops after the first form, so `"2 + 2"` would evaluate to just `2`, and any text reaching `eval` is a code-execution path. Malformed or rejected input is caught by `handler-case` and returned as an `"Error calculating: ..."` string, which the tool loop hands back to the model so it can correct itself on the next turn.

Sample REPL session:

```lisp
* (ql:quickload :ollama)
To load "ollama":
  Load 1 ASDF system:
    ollama
; Loading "ollama"
[package ollama].

* (ollama:completions-with-tools "Use the get_weather tool for: What's the weather like in New York?" '("get_weather" "calculate"))
Raw response: (("id" . "chatcmpl-336") ("object" . "chat.completion")
               ("created" . 1789576838) ("model" . "qwen3-vl:2b")
               ("system_fingerprint" . "fp_ollama")
               ("choices"
                (("index" . 0)
                 ("message" ("role" . "assistant") ("content" . "")
                  ("reasoning"
                   . "Okay, the user is asking about the weather in New York. ...")
                  ("tool_calls"
                   (("id" . "call_g0yphv05") ("index" . 0)
                    ("type" . "function")
                    ("function" ("name" . "get_weather")
                     ("arguments" . "{\"location\":\"New York\"}")))))
                 ("finish_reason" . "tool_calls")))
               ("usage" ("prompt_tokens" . 238) ("completion_tokens" . 103) ("total_tokens" . 341)))

Model requested 1 tool call(s).

DEBUG handle-tool-function-call: (ID call_g0yphv05 NAME get_weather ARGUMENTS ((LOCATION . New York)))
DEBUG raw-name=get_weather inferred-name=get_weather args=((LOCATION . New York)) func=#S(OLLAMA-FUNCTION :NAME get_weather ...)
get_weather called with args: ((LOCATION . New York))
  Tool get_weather completed.
Raw response: (("id" . "chatcmpl-200") ("object" . "chat.completion")
               ("created" . 1789576839) ("model" . "qwen3-vl:2b")
               ("system_fingerprint" . "fp_ollama")
               ("choices"
                (("index" . 0)
                 ("message" ("role" . "assistant")
                  ("content"
                   . "The weather in New York is currently **Sunny** with a temperature of **72°F**. Enjoy your day! 😊")
                  ("reasoning"
                   . "Okay, the user asked for the weather in New York. ..."))
                 ("finish_reason" . "stop")))
               ("usage" ("prompt_tokens" . 283) ("completion_tokens" . 153) ("total_tokens" . 436)))

"The weather in New York is currently **Sunny** with a temperature of **72°F**. Enjoy your day! 😊"
```

Note that the model is called twice. The first turn returns a `tool_calls` finish reason and no prose, so `completions-with-tools` executes `get_weather` and appends both the assistant turn and the tool result to `messages`. The second turn then has the tool output available and returns the final natural-language answer with a `stop` finish reason, which ends the loop. When a single turn requests several tools (for example, asking for both the weather and a calculation), the `dolist` dispatches each one and appends one `:tool` message per call before the next model call.


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

4. **Bounding the Tool Loop**:
   The loop in `completions-with-tools` in [ollama-tools.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama-tools.lisp) repeats until the model stops requesting tools, with no upper bound. Add a `:max-iterations` keyword argument (for example, defaulting to 8) and, when the limit is reached, stop and return the most recent assistant content together with a diagnostic noting that the limit was hit. Verify that a prompt engineered to keep requesting tools terminates instead of looping indefinitely.

5. **Tool Error Handling and Recovery**:
   An uncaught error anywhere in the dispatch aborts the entire tool loop. `handle-tool-function-call` signals an error for an unregistered tool name, and a handler may throw for any other reason. Wrap the handler dispatch in [ollama-tools.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama-tools.lisp) in `handler-case` and return a descriptive message string as the tool result instead. Then exercise the path — for example, register a tool whose handler deliberately signals an error — and observe how the LLM recovers in the multi-turn loop. (Note that `calculate` already catches its own arithmetic errors such as division by zero, so pick a different failure for this exercise.)

6. **Web Search Fallback for Local Models**:
   Combine the tool-calling mechanism of `completions-with-tools` from [ollama-tools.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama-tools.lisp) and the DuckDuckGo query mechanism of `execute-web-search`/`execute-web-fetch` from [ollama-cloud-search.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/ollama/ollama-cloud-search.lisp) to run entirely on a local model (such as `qwen3-vl:2b` or `qwen3.5:2b`). Register the web tools locally and test how effectively small local models can perform multi-turn search queries.
