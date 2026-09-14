# More Agents Using X’s Grok and Perplexity APIs

One of the joys of working in Common Lisp is how naturally it lends itself to building extensible agent systems. We can represent knowledge symbolically, apply reasoning rules, and integrate procedural code, all within one coherent runtime. Over the years, I’ve experimented with many ways of connecting Lisp-based reasoning systems to external AI services, from symbolic logic engines to modern large language models (LLMs). In this chapter, we’ll take that a step further by exploring two complementary APIs that allow our Lisp agents to both reason and learn from the world in real time: X’s Grok LLM and the Perplexity Web Search API.

### Why Grok?

Grok, the LLM developed and maintained by X (formerly Twitter), provides a conversational and reasoning-capable API similar to other large language models but with a twist: it’s designed for real-time access to context and access to current data through X’s ecosystem. While Grok is still an evolving platform, it’s particularly interesting to Lisp developers because it can be treated as a remote reasoning component, an external “mind” that our Lisp agent can consult for pattern completion, text summarization, or general problem solving.

In the first example of this chapter, we’ll look at the simplest possible integration: a Lisp program that sends a prompt to the Grok API and uses a single, very basic tool: a Lisp function called **get_current_date**. The tool simply returns the current date and time in a human-readable format. While this might seem trivial, it serves an example to demonstrate how to:

- Define a Lisp-side function as an external tool the model can call.
- Serialize and pass structured information between Lisp and Grok.
- Maintain conversational context between model invocations.

This minimal setup provides a foundation for richer tool-using agents later on. Once the pattern is clear, a model prompt, tool definition, and response interpretation, then we can add more tools or swap in different LLM backends without changing the surrounding Lisp logic.

### From Reasoning to Knowledge: Adding Perplexity Search

The second example expands the system into something more dynamic. Instead of relying solely on the Grok model’s internal knowledge, we connect to the Perplexity API, which acts as an intelligent web search layer. Perplexity’s model performs real-time retrieval from the web, returning concise, cited answers. Combined with Grok, this gives our Lisp agent two distinct reasoning modalities:

- Generative reasoning through Grok: language understanding, summarization, creative or speculative reasoning.
- Retrieval reasoning through Perplexity: grounded, factual responses based on live web content.

This dual setup mirrors the way human researchers work: we think abstractly, but we also look things up. By orchestrating these two APIs from Lisp, we can build an agent that decides when to “ask” Grok for interpretation versus when to query Perplexity for up-to-date information. The Lisp runtime remains the central coordinator, maintaining context and deciding when and how to merge results.

### A Unified Lisp Interface for Multiple Cognitive Modes

In both examples, the Common Lisp code will share a similar structure. We’ll define a small framework for:

- Representing API requests and responses as Lisp objects.
- Managing authentication and HTTP requests.
- Logging and tracing agent conversations for debugging and reuse.

The goal isn’t to build a full tool calling agent abstraction layer for all LLM APIs, but to provide a reusable pattern for experimentation. Lisp’s macro system, combined with its symbolic data structures, makes it easy to treat prompts and API calls as first-class objects, allowing us to script agent workflows that feel like extensions of the language itself.

## Agent Using X’s Grok API

The program's architecture is centered around a few key components that manage the agent's capabilities and state. Configuration for the Grok API is handled by the global variable **X_GROK_API_KEY**, while the agent's extensible skills are stored in the **tools** hash table. This hash table serves as a registry, mapping tool names to their description, parameter schema, and the actual Lisp function that implements the tool's logic. The **def-tool** macro provides a clean, declarative syntax for populating this registry, abstracting away the JSON schema details required by the API and making it simple for developers to add new capabilities. A small helper function, **hash**, is also included to simplify the creation of nested hash tables that are later serialized into the JSON format expected by the Grok service.

The agent's operational logic resides in the **run-agent** function, which implements a conversational loop. It begins by constructing an initial list of messages, including the user's query and an optional system prompt. In each iteration of the loop, it calls **call-grok-chat**, which sends the current conversation history and the list of available tools to the Grok API. The agent then inspects the model's response. If the model's **finish_reason** indicates it wants to call a tool, the code extracts the tool name and arguments, invokes the corresponding Lisp function via **execute-tool**, and appends the tool's output to the message history. This new history is then sent back to the model in the next loop iteration. This cycle continues until the model generates a final textual answer, at which point the loop terminates and returns the result.

File **agent.lisp**:

```lisp
;;;; agent-system.lisp
;;;; A Common Lisp agent system using Grok API with support for tool calling.
;;;;
;;;; Dependencies (load via Quicklisp):
;;;;   (ql:quickload '(:drakma :yason :alexandria :uiop :cl+ssl))
;;;;
;;;; Usage:
;;;;   Set *grok-api-key* to your xAI Grok API key.
;;;;   Define custom tools using def-tool.
;;;;   Run (run-agent "Your query here")
;;;;
;;;; Note: This assumes Grok API is compatible with OpenAI-style chat completions.

(in-package :cl-user)

(ql:quickload '(:drakma :yason :alexandria :uiop :cl+ssl))

;; Required libraries
(require 'asdf)
(require 'uiop)

;; Configure YASON to handle symbol keys & values
(setf yason:*symbol-encoder* #'yason:encode-symbol-as-string)

(defvar *grok-api-key*
  (uiop:getenv "X_GROK_API_KEY")
  "Your xAI Grok API key. Obtain from https://x.ai/api")

(defvar *grok-base-url* "https://api.x.ai/v1"
  "Base URL for Grok API.")

(defvar *tools* (make-hash-table :test 'equal)
  "Hash table of tools: name -> (description parameters lisp-function)")

(defun hash (&rest pairs)
  "Helper to create hash-table from pairs. Converts symbol or keyword keys to lowercase strings so YASON sees only string keys."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          for key = (if (symbolp k)
                        (string-downcase (symbol-name k))
                        k)
          do (setf (gethash key ht) v))
    ht))

(defmacro def-tool (name description parameters lisp-function)
  "Define a custom tool."
  `(setf (gethash ,name *tools*)
         (list ,description ,parameters ,lisp-function)))

;; Example custom tool: get current date
(def-tool "get_current_date"
  "Get the current date in YYYY-MM-DD format."
  (hash :type "object" :properties (hash) :required #())
  (lambda (args)
    (declare (ignore args))
    (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
      (declare (ignore s m h))
      (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d))))

;; Function to get tools in API format
(defun get-tools ()
  "Return list of tool schemas for API."
  (loop for name being the hash-keys of *tools*
        collect (destructuring-bind (desc params fn)
                    (gethash name *tools*)
                  (declare (ignore fn))
                  (hash "type" "function"
                        "function" (hash "name" name
                                         "description" desc
                                         "parameters" params)))))

(defun call-grok-chat (messages &key (model "grok-4.20-non-reasoning") tools)
  (let ((body (hash "model"     model
                    "messages"  messages
                    "stream"    yason:false)))
    (when tools (setf (gethash "tools" body) tools))
    (let* ((json-body (with-output-to-string (s) (yason:encode body s)))
           (status nil) (raw nil))
      (multiple-value-setq (raw status)
        (drakma:http-request
         (concatenate 'string *grok-base-url* "/chat/completions")
         :method :post
         :additional-headers
           `(("Authorization" . ,(concatenate 'string "Bearer " *grok-api-key*)))
         :content       json-body
         :content-type  "application/json"
         :verify nil))
      (unless (= status 200)
        (error "Grok API returned status ~a: ~a" status raw))
      (let* ((body-str (if (vectorp raw)
                           (babel:octets-to-string raw :encoding :utf-8)
                           raw))
             (parsed   (yason:parse body-str)))
        parsed))))

(defun execute-tool (tool-call)
  "Execute a tool call and return the result string (or hash) from the invoked tool."
  (let* ((function-info (gethash "function" tool-call))
         (name          (gethash "name" function-info))
         (args-raw      (gethash "arguments" function-info))

         ;; Force ARGS-JSON to a true simple-string
         (args-json
           (cond
             ;; Character vector → simple-string
             ((and (vectorp args-raw) (every #'characterp args-raw))
              (coerce args-raw 'simple-string))

             ;; Already a string → coerce to simple-string to drop any adjustable/ fill‑pointer baggage
             ((stringp args-raw)
              (coerce args-raw 'simple-string))

             ;; Octet vector → decode UTF‑8
             ((vectorp args-raw)
              (babel:octets-to-string args-raw :encoding :utf-8))

             (t
              (error "Unexpected arguments payload type: ~s" (type-of args-raw)))))

         (tool-info (gethash name *tools*)))
    ;; DEBUG PRINTS ----------------------------------------------------------
    (format t "~&[execute-tool] name=~a args-raw type=~a~%" name (type-of args-raw))
    (cond
      ((stringp args-raw)
       (format t "[execute-tool] first 32 chars: ~a~%"
               (subseq args-raw 0 (min 32 (length args-raw)))))
      ((and (vectorp args-raw) (not (stringp args-raw)))
       (format t "[execute-tool] first 16 bytes: ~{~d~^ ~}~%"
               (subseq args-raw 0 (min 16 (length args-raw))))))
    (format t "[execute-tool] args-json final type=~a first 32: ~a~%"
            (type-of args-json)
            (subseq args-json 0 (min 32 (length args-json))))
    ;; ----------------------------------------------------------------------
    (let* ((args         (yason:parse args-json)))
      (if tool-info
          (let ((fn (third tool-info)))
            (funcall fn args))
          (error "Unknown tool: ~s" name)))))

(defun run-agent (query &key (model "grok-4.20-non-reasoning") (system-prompt "You are a helpful agent that can use tools to answer questions."))
  "Run the agent loop for a query."
  (let ((messages (if system-prompt
                      (list (hash "role" "system" "content" system-prompt)
                            (hash "role" "user" "content" query))
                      (list (hash "role" "user" "content" query))))
        (tools (get-tools)))
    (loop
      (let ((response (call-grok-chat messages :model model :tools tools)))
        (let* ((choice (first (gethash "choices" response)))
               (message (gethash "message" choice))
               (finish-reason (gethash "finish_reason" choice)))
          (push message messages)  ;; Add assistant message to history
          (cond
            ;; Tool invocation (either explicit finish_reason or implicit
            ;; via presence of tool_calls)
            ((or (member finish-reason '("tool_calls" "tool_call") :test #'equal)
                 (gethash "tool_calls" message))
             (let ((tool-calls (gethash "tool_calls" message)))
               (dolist (tool-call tool-calls)
                 (let* ((result (execute-tool tool-call))
                        (tool-response (hash "role" "tool"
                                             "tool_call_id" (gethash "id" tool-call)
                                             "name" (gethash "name" (gethash
                                                      "function" 
                                                      tool-call))
                                             "content" result)))
                   (push tool-response messages)))))

            ;; Conversation finished
            ((or (equal finish-reason "stop")
                 ;; finish_reason NIL/"" --> stop only if no tool_calls present
                 (and (or (null finish-reason) (equal finish-reason ""))
                      (not (gethash "tool_calls" message))))
             (return (gethash "content" message)))

            (t
             (error "Unknown finish reason: ~s" finish-reason))))))))

(trace call-grok-chat)
(trace execute-tool)
(trace get-tools)

;; (run-agent "what is 1 + 12?")
;; (run-agent "Consultant Mark Watson has written books on AI, Lisp, and the semantic web. What musical instruments does Mark play?")
```

This program provides a functional and concise foundation for building intelligent agents that can take action in the world. By combining the classic strengths of Common Lisp with the modern capabilities of the Grok API, it demonstrates a powerful pattern for creating tool-augmented AI systems. The **def-tool** macro, in particular, offers a clear path for extension, allowing a developer to easily equip the agent with a wide array of custom functions, from interacting with databases and other APIs to controlling local system processes. This example serves mostly as a demonstration, but if customized for your agent requirements it can also be a robust starting point for developing more sophisticated and specialized AI applications in Lisp.

Let’s run the two examples at the bottom of the last listing:

```lisp
CL-USER 1 > (load "agent.lisp")
CL-USER 2 > (run-agent "what is 1 + 12?")
0 GET-TOOLS > ...
0 GET-TOOLS < ...
  << VALUE-0 : (#<EQUAL Hash Table{2} 801002623B>)
0 CALL-GROK-CHAT > ...
  >> MESSAGES : (#<EQUAL Hash Table{2} 8010017C03> #<EQUAL Hash Table{2} 801001A013>)
  >> MODEL    : "grok-4.20-non-reasoning"
  >> TOOLS    : (#<EQUAL Hash Table{2} 801002623B>)
0 CALL-GROK-CHAT < ...
  << VALUE-0 : #<EQUAL Hash Table{7} 801009F0E3>
"13"
T

CL-USER 3 > (run-agent "Consultant Mark Watson has written books on AI, Lisp, and the semantic web.")
0 GET-TOOLS > ...
0 GET-TOOLS < ...
  << VALUE-0 : (#<EQUAL Hash Table{2} 80100AF723>)
0 CALL-GROK-CHAT > ...
  >> MESSAGES : (#<EQUAL Hash Table{2} 80100A23CB> #<EQUAL Hash Table{2} 80100A4763>)
  >> MODEL    : "grok-4.20-non-reasoning"
  >> TOOLS    : (#<EQUAL Hash Table{2} 80100AF723>)
0 CALL-GROK-CHAT < ...
  << VALUE-0 : #<EQUAL Hash Table{7} 80100CFF4B>
"Yes, that's accurate! Mark Watson is a software consultant and author with a strong background in artificial intelligence, programming languages like Common Lisp, and technologies such as the Semantic Web. Some of his notable books include:

- **Loving Common Lisp, or the Savvy Programmer's Secret Weapon** (on Lisp programming).
- **Practical Semantic Web and Linked Data Applications** (focusing on Semantic Web technologies).
- **Practical Artificial Intelligence Programming With Java** (covering AI concepts).

He's written over 20 books in total, often emphasizing practical, hands-on approaches to these subjects. If you're interested in recommendations, specific book details, or more about his work, let me know!"
T

CL-USER 4 >
```

Here we are using the innate knowledge in X’s Grok model.

## Agent Using X’s Grok API and Perplexity’s Search API

Here we extend the example in the last section to use a web search tool implemented with Perplexity's web search API through the shared **search-apis** library from the earlier web search chapter.

The architecture of this example tool using agent is centered around the **run-agent** function, which implements the core reasoning loop. It begins by sending the user's query and a list of available tools to the Grok API. The program then inspects the API response's **finish_reason**. If Grok determines a tool is needed, the reason will be in **tool_calls**, and the response will contain the name of the tool to execute and the arguments to use. The **execute-tool** function then dispatches to the appropriate local Lisp function. The tool's output is then packaged into a new message and sent back to Grok, continuing the loop. This cycle repeats until Grok has sufficient information and returns a **finish_reason** of stop, at which point it delivers its final synthesized answer to the user.

The system's extensibility is handled by the **def-tool** macro that creates a simple domain-specific language for adding new tools with specified capabilities. To define a new tool, a developer provides its name, a natural language description for the LLM to understand its purpose, a JSON schema for its parameters, and the Lisp lambda function that performs the actual work. The **web_search** tool is an example, as it acts as a bridge to another AI service, Perplexity. Instead of performing a raw web search, it effectively asks the Perplexity Sonar model to answer the query, ensuring the result returned to Grok is a concise, relevant summary. This demonstrates a powerful pattern of chaining specialized AI models together within a single agentic framework. The tool is implemented with one call to **search-apis:websearch** (provider **:perplexity**), so all of the Perplexity-specific HTTP and JSON handling lives in the shared search library. Communication with the Grok API is managed by the Drakma library for HTTP requests and the YASON library for handling the necessary JSON serialization and parsing.

This agent example is a work in progress and currently running the agent results in hundreds of lines of debug printout.


The following diagram shows the high-level architecture of the Grok and Perplexity agent system developed in this chapter:

{width: "80%"}
![Architecture diagram](images/agent_part2_architecture.png)

File **agent_grok_perplexity.lisp**:

```lisp
;;;; agent-system.lisp
;;;; A Common Lisp agent system using Grok API with support for tool calling.
;;;; Uses the shared search-apis library (../search_APIs) with its Perplexity
;;;; provider for the web search tool.
;;;;
;;;; Dependencies (load via Quicklisp):
;;;;   (ql:quickload '(:drakma :yason :alexandria :uiop :cl+ssl))
;;;; The search-apis system (dexador + quri only) must also be loadable, e.g.:
;;;;   (asdf:load-asd (merge-pathnames "search_APIs/search-apis.asd" *load-pathname*))
;;;;   (asdf:load-system :search-apis)
;;;;
;;;; Usage:
;;;;   Set *grok-api-key* to your xAI Grok API key.
;;;;   Set the PERPLEXITY_API_KEY environment variable for web search support.
;;;;   Define custom tools using def-tool.
;;;;   Run (run-agent "Your query here")
;;;;
;;;; Note: This assumes Grok API is compatible with OpenAI-style chat completions.

(in-package :cl-user)

(ql:quickload '(:drakma :yason :alexandria :uiop :cl+ssl))

;; Required libraries
(require 'asdf)
(require 'uiop)

;; Load the shared search-apis library (used for the web_search tool):
(let ((asd (merge-pathnames "../search_APIs/search-apis.asd"
                            (or *load-pathname* *default-pathname-defaults*))))
  (when (probe-file asd)
    (asdf:load-asd asd)))
(asdf:load-system :search-apis)

;; Configure YASON to handle symbol keys & values
(setf yason:*symbol-encoder* #'yason:encode-symbol-as-string)

(defvar *grok-api-key*
  (uiop:getenv "X_GROK_API_KEY")
  "Your xAI Grok API key. Obtain from https://x.ai/api")

(defvar *grok-base-url* "https://api.x.ai/v1"
  "Base URL for Grok API.")

(defvar *perplexity-model* "sonar"
  "Perplexity search-plus-LLM model used by the web_search tool.")

(defvar *tools* (make-hash-table :test 'equal)
  "Hash table of tools: name -> (description parameters lisp-function)")

(defun hash (&rest pairs)
  "Helper to create hash-table from pairs. Converts symbol or keyword keys to lowercase strings so YASON sees only string keys."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          for key = (if (symbolp k)
                        (string-downcase (symbol-name k))
                        k)
          do (setf (gethash key ht) v))
    ht))

(defun pp-hash (ht &optional (stream *standard-output*) (indent 0))
  "Pretty-print hash table HT to STREAM, indenting by INDENT spaces."
  (let ((keys (loop for k being the hash-keys of ht collect k)))
    (format stream "~&~v@{~}" indent "")     ; indent
    (format stream "#HASH{~%")
    (let ((next-indent (+ indent 2)))
      (dolist (k keys)
        (let ((v (gethash k ht)))
          (format stream "~v@{~}" next-indent "")
          (format stream "~S => ~S~%" k v)))
      (format stream "~v@{~}" indent "")
      (format stream "}") )
  ht))

(defmacro def-tool (name description parameters lisp-function)
  "Define a custom tool."
  `(setf (gethash ,name *tools*)
         (list ,description ,parameters ,lisp-function)))

;; Example tools

;; Web search tool using the shared search-apis library (Perplexity provider)
(def-tool "web_search"
  "Search the web for up-to-date information when needed. Use this for current events or real-time data."
  (hash :type "object"
        :properties (hash "query" (hash :type "string"
                                        :description "The search query string."))
        :required (list "query"))
  (lambda (args)
    (let ((query (gethash "query" args)))
      (handler-case
          (let ((response (search-apis:websearch query
                                                 :provider :perplexity
                                                 :model *perplexity-model*)))
            ;; Perplexity returns a synthesized answer plus cited sources;
            ;; return the answer to Grok, appending the source URLs.
            (format t "~&[web_search] Perplexity answer=~%~A~%"
                    (search-apis:search-response-answer response))
            (with-output-to-string (s)
              (when (search-apis:search-response-answer response)
                (write-string (search-apis:search-response-answer response) s))
              (dolist (r (search-apis:search-response-results response))
                (format s "~%Source: ~A" (search-apis:search-result-url r)))))
        (search-apis:search-error (e)
          (format nil "Web search failed: ~A" e))))))

;; Example custom tool: get current date
(def-tool "get_current_date"
  "Get the current date in YYYY-MM-DD format."
  (hash :type "object" :properties (hash) :required #())
  (lambda (args)
    (declare (ignore args))
    (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
      (declare (ignore s m h))
      (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d))))

;; Function to get tools in API format
(defun get-tools ()
  "Return list of tool schemas for API."
  (loop for name being the hash-keys of *tools*
        collect (destructuring-bind (desc params fn)
                    (gethash name *tools*)
                  (declare (ignore fn))
                  (hash "type" "function"
                        "function" (hash "name" name
                                         "description" desc
                                         "parameters" params)))))

(defun call-grok-chat (messages &key (model "grok-4.20-non-reasoning") tools)
  (let ((body (hash "model"     model
                    "messages"  messages
                    "stream"    yason:false)))
    (when tools (setf (gethash "tools" body) tools))
    (let* ((json-body (with-output-to-string (s) (yason:encode body s)))
           (status nil) (raw nil))
      (multiple-value-setq (raw status)
        (drakma:http-request
         (concatenate 'string *grok-base-url* "/chat/completions")
         :method :post
         :additional-headers
           `(("Authorization" . ,(concatenate 'string "Bearer " *grok-api-key*)))
         :content       json-body
         :content-type  "application/json"
         :verify nil))
      (unless (= status 200)
        (error "Grok API returned status ~a: ~a" status raw))
      (let* ((body-str (if (vectorp raw)
                           (babel:octets-to-string raw :encoding :utf-8)
                           raw))
             (parsed   (yason:parse body-str)))
        parsed))))

(defun execute-tool (tool-call)
  "Execute a tool call and return the result string (or hash) from the invoked tool."
  (let* ((function-info (gethash "function" tool-call))
         (name          (gethash "name" function-info))
         (args-raw      (gethash "arguments" function-info))

         ;; Force ARGS-JSON to a true simple-string
         (args-json
           (cond
             ;; Character vector --> simple-string
             ((and (vectorp args-raw) (every #'characterp args-raw))
              (coerce args-raw 'simple-string))

             ;; Already a string --> coerce to simple-string to drop any
             ;; adjustable/fill-pointer baggage
             ((stringp args-raw)
              (coerce args-raw 'simple-string))

             ;; Octet vector → decode UTF‑8
             ((vectorp args-raw)
              (babel:octets-to-string args-raw :encoding :utf-8))

             (t
              (error "Unexpected arguments payload type: ~s" (type-of args-raw)))))

         (tool-info (gethash name *tools*)))
    ;; DEBUG PRINTS ----------------------------------------------------------
    (format t "~&[execute-tool] name=~a args-raw type=~a~%" name (type-of args-raw))
    (cond
      ((stringp args-raw)
       (format t "[execute-tool] first 32 chars: ~a~%"
               (subseq args-raw 0 (min 32 (length args-raw)))))
      ((and (vectorp args-raw) (not (stringp args-raw)))
       (format t "[execute-tool] first 16 bytes: ~{~d~^ ~}~%"
               (subseq args-raw 0 (min 16 (length args-raw))))))
    (format t "[execute-tool] args-json final type=~a first 32: ~a~%"
            (type-of args-json)
            (subseq args-json 0 (min 32 (length args-json))))
    ;; ----------------------------------------------------------------------
    (let* ((args         (yason:parse args-json)))
      (if tool-info
          (let ((fn (third tool-info)))
            (funcall fn args))
          (error "Unknown tool: ~s" name)))))

(defun run-agent (query &key (model "grok-4.20-non-reasoning") (system-prompt "You are a helpful agent that can use tools to answer questions."))
  "Run the agent loop for a query."
  (let ((messages (if system-prompt
                      (list (hash "role" "system" "content" system-prompt)
                            (hash "role" "user" "content" query))
                      (list (hash "role" "user" "content" query))))
        (tools (get-tools)))
    (loop
      (let ((response (call-grok-chat messages :model model :tools tools)))
        (let* ((choice (first (gethash "choices" response)))
               (message (gethash "message" choice))
               (finish-reason (gethash "finish_reason" choice)))
          (push message messages)  ;; Add assistant message to history
          (cond
            ;; Tool invocation (either explicit finish_reason or implicit
            ;; via presence of tool_calls)
            ((or (member finish-reason '("tool_calls" "tool_call") :test #'equal)
                 (gethash "tool_calls" message))
             (let ((tool-calls (gethash "tool_calls" message)))
               (dolist (tool-call tool-calls)
                 (let* ((result (execute-tool tool-call))
                        (tool-response (hash "role" "tool"
                                             "tool_call_id" (gethash "id" tool-call)
                                             "name" (gethash "name" (gethash
                                                      "function" 
                                                      tool-call))
                                             "content" result)))
                   (push tool-response messages)))))

            ;; Conversation finished
            ((or (equal finish-reason "stop")
                 ;; finish_reason NIL/"" → stop only if no tool_calls present
                 (and (or (null finish-reason) (equal finish-reason ""))
                      (not (gethash "tool_calls" message))))
             (return (gethash "content" message)))

            (t
             (error "Unknown finish reason: ~s" finish-reason))))))))

(trace call-grok-chat)
(trace execute-tool)
(trace get-tools)

;; (run-agent "what is 1 + 12?")
;; (run-agent "Consultant Mark Watson has written books on AI, Lisp, and the semantic web. What musical instruments does Mark play? Return only a list of musical instruments.")
```

Let’s run an example that requires a web search since Grok’s innate knowledge can’t answer the query:

```
CL-USER 1 > (load "agent_grok_perplexity.lisp")
CL-USER 3 > (run-agent "Consultant Mark Watson has written books on AI, Lisp, and the semantic web. What musical instruments does Mark play? Return only a list of musical instruments.")
0 GET-TOOLS > ...
0 GET-TOOLS < ...
  << VALUE-0 : (#<EQUAL Hash Table{2} 801002345B> #<EQUAL Hash Table{2} 801002A9DB>)
0 CALL-GROK-CHAT > ...
  >> MESSAGES : (#<EQUAL Hash Table{2} 8010015043> #<EQUAL Hash Table{2} 80100173DB>)
  >> MODEL    : "grok-4.20-non-reasoning"
  >> TOOLS    : (#<EQUAL Hash Table{2} 801002345B> #<EQUAL Hash Table{2} 801002A9DB>)
0 CALL-GROK-CHAT < ...
  << VALUE-0 : #<EQUAL Hash Table{7} 80100A46EB>
0 EXECUTE-TOOL > ...
  >> TOOL-CALL : #<EQUAL Hash Table{3} 80100A625B>
[execute-tool] name=web_search args-raw type=(ARRAY CHARACTER (80))
[execute-tool] first 32 chars: {"query":"Mark Watson AI Lisp se
[execute-tool] args-json final type=SIMPLE-TEXT-STRING first 32: {"query":"Mark Watson AI Lisp se
[web_search] Perplexity answer=
Mark Watson is an AI practitioner, author, and programmer specializing in Lisp, semantic web, and large language models. He is the author of numerous books on AI, Lisp, semantic web technologies, and programming languages. He has worked extensively on semantic web and linked data applications, including a Common Lisp version of his semantic web book, and he integrates AI tools like OpenAI GPT and LangChain in his work[1][5][6].

Regarding musical instruments, Mark Watson plays the **guitar, didgeridoo, and American Indian flute** as part of his hobbies[2].

In summary:

| Aspect                     | Details                                               |
|----------------------------|-------------------------------------------------------|
| Profession                 | AI practitioner, Lisp programmer, semantic web author |
| Key Contributions          | Books on AI, Lisp, semantic web; projects using Lisp and AI |
| Semantic Web Work          | Practical Semantic Web and Linked Data Applications (Common Lisp and others) |
| Musical Instruments Played | Guitar, didgeridoo, American Indian flute             |

This information is based on Mark Watson's personal website, books, and profiles[1][2][5][6].
0 EXECUTE-TOOL < ...
  << VALUE-0 : "Mark Watson is an AI practitioner, author, and programmer specializing in Lisp, semantic web, and large language models. He is the author of numerous books on AI, Lisp, semantic web technologies, and programming languages. He has worked extensively on semantic web and linked data applications, including a Common Lisp version of his semantic web book, and he integrates AI tools like OpenAI GPT and LangChain in his work[1][5][6].

Regarding musical instruments, Mark Watson plays the **guitar, didgeridoo, and American Indian flute** as part of his hobbies[2].

In summary:

| Aspect                     | Details                                               |
|----------------------------|-------------------------------------------------------|
| Profession                 | AI practitioner, Lisp programmer, semantic web author |
| Key Contributions          | Books on AI, Lisp, semantic web; projects using Lisp and AI |
| Semantic Web Work          | Practical Semantic Web and Linked Data Applications (Common Lisp and others) |
| Musical Instruments Played | Guitar, didgeridoo, American Indian flute             |

This information is based on Mark Watson's personal website, books, and profiles[1][2][5][6]."
0 CALL-GROK-CHAT > ...
  >> MESSAGES : (#<EQUAL Hash Table{4} 80100DE1A3> #<EQUAL Hash Table{4} 80100A5F73> #<EQUAL Hash Table{2} 8010015043> #<EQUAL Hash Table{2} 80100173DB>)
  >> MODEL    : "grok-4.20-non-reasoning"
  >> TOOLS    : (#<EQUAL Hash Table{2} 801002345B> #<EQUAL Hash Table{2} 801002A9DB>)
0 CALL-GROK-CHAT < ...
  << VALUE-0 : #<EQUAL Hash Table{7} 80100EA7C3>
"- Guitar
- Didgeridoo
- American Indian flute"
T

CL-USER 4 > 
```

This tool using agent is a work in progress so I left debug output in place.