# Agents Orchestrating LLM Tool Use

AI agents have emerged as a powerful abstraction for tackling complex tasks by breaking them down into manageable, goal-oriented units. Unlike monolithic systems, agents are modular, autonomous, and capable of acting independently to achieve specific objectives. This flexibility makes them particularly well-suited for integrating with modern AI capabilities like large language models (LLMs). But why agents? The answer lies in their ability to simplify complex workflows and adapt dynamically to evolving requirements using the innate knowledge in LLMs. By isolating specific tasks into distinct entities, agents enable parallel processing, enhanced fault tolerance, and improved maintainability in AI systems. Moreover, when paired with the reasoning and contextual understanding of LLMs, agents can unlock new levels of problem-solving sophistication.

LLMs, with their surprisingly good ability to understand and generate human-like text, are revolutionizing the way AI interacts with the world. However, LLMs are autoregressive models and base their output solely on preceding prompts and generated text: they not all-powerful but they can often be supplemented with external tools and APIs to solve real-world problems effectively. For instance, while an LLM might generate code, it still needs an external agent to execute it, evaluate the results, and determine the next steps. This is where tool use comes in. Agents that leverage LLMs can be designed to interface with external systems like databases, REST APIs, search engines, and even hardware devices. By integrating LLMs with specialized tools, agents become capable of reasoning, planning, and acting on real-world data in ways that go far beyond simple text generation.

Orchestrating multiple agents, however, introduces a new layer of complexity. Which agents should be allowed to communicate with each other? How do we prevent cascading errors or infinite loops in agent interactions? A carefully designed orchestration system ensures that agents collaborate effectively while respecting boundaries. For instance, you might design a system where a planning agent calls a query agent to fetch data but disallows it from calling agents responsible for financial transactions. Similarly, certain agents might act as supervisors, verifying outputs or resolving conflicts between other agents. This chapter explores strategies for designing such orchestration layers, with a focus on managing dependencies, optimizing communication flows, and leveraging the unique strengths of individual agents to create cohesive, intelligent systems. We wrap up these ideas in an agent framework and several example tools.

## Example Multi Agent Implementation

The example in this chapter relies on the code in the last two chapters:

- The Google Gemini client code in the repository [https://github.com/mark-watson/gemini/](https://github.com/mark-watson/gemini/).
- The Tavily client code in the repository [https://github.com/mark-watson/tavily](https://github.com/mark-watson/tavily).

As usual you want to git clone these two repository in your local directory **~/quicklisp/local-projects/** so Quicklisp can, for example, find these libraries with **(ql:quickload :gemini)** and **(ql:quickload :tavily)**.

You can find the source code for the example in this chapter in the GitHub repository [https://github.com/mark-watson/cl-llm-agent](https://github.com/mark-watson/cl-llm-agent). I suggest that you also git clone this example project in your local directory **~/quicklisp/local-projects/**.

The example implementation of a multi agent system consists of three Common Lisp source files and one ASDF file (listed in the order of later discussion):

- package.lisp
- cl-llm-agent.asd
- agent.lisp
- test.lisp

In the following sections the code is listed first followed by a discussion.

### package.lisp

```lisp
(defpackage :cl-llm-agent
  (:use :cl)
  (:export #:define-agent
           #:make-agent
           #:agent-converse
           #:agent-llm-call
           #:make-context
           #:context-data
           #:context-set
           #:context-get
           #:context-remove
           #:display-context
           #:register-tool
           #:list-tools
           #:execute-tool
           #:agent-tools
           #:agent-context
           #:*agent-verbose*))
```

This package.lisp file defines the structure and organization of this example project that builds an agent system capable of interacting with language models (Gemini), performing web searches (Tavily), and executing various user-defined tools. It uses packages to encapsulate functionalities and manage dependencies.

### cl-llm-agent.asd

```lisp
;;; cl-llm-agent.asd -- ASDF system definition for cl-llm-agent
(in-package #:asdf-user)

(defsystem "cl-llm-agent"
  :name "cl-llm-agent"
  :version "0.1.0"
  :author "Mark Watson <markw@markwatson.com>"
  :license "MIT"
  :description "A generic LLM-based agent framework for Common Lisp."
  :depends-on ("cl-json" "gemini" "tavily" "uiop" "fiveam")
  :components ((:file "package")
               (:file "context")
               (:file "agent-generic")
               (:file "agent-gemini")
               (:file "tools")))
```
               
### agent-gemini.lisp agent-generic.lisp context.lisp tools.lisp

The code in this chapter was refactored May 15, 2025.

We use these predefined tools:

**Tool Registry:**

- ***tool-registry***: A hash table that stores tool definitions. It uses strings for equality comparisons (:test #'equal).

**Directory Handling Utilities:**

- directory-pathname-p: A helper function that checks if a given path name is a directory.
- probe-directory: Checks if a given path name is an existing directory.

**Tool Definition and Registration:**

- define-tool: A macro that simplifies tool definition. It takes the tool's name, description, parameters, parameter-example, and a-function (the function that implements the tool's logic). It expands into a call to register-tool.
- register-tool: A function that registers a tool in the *tool-registry*. It stores the tool's name, description, parameters, an example of how to use the tool's parameters, and the implementing function in a list within the hash table.

**Tool Execution:**

- execute-tool: A function that executes a registered tool.
  - It retrieves the tool's data from *tool-registry*.
  - It checks if the tool exists and if it has a defined function.
  - It performs a basic check to see if the number of arguments provided matches the number of parameters the tool expects.
  - It uses apply to call the tool's function with the provided arguments.
  - It includes error handling for cases where the tool is not found, has no function, or the number of arguments is incorrect.

**Tool Listing:**

- list-tools: A function that returns a list of registered tools, including their names and descriptions.

**Example Tool Definitions:**

- helper-tool-read-directory: Lists files in a directory (excluding emacs backup files).
- tool-read-directory: A tool defined using define-tool that uses helper-tool-read-directory to read the contents of a directory specified by the directory-path parameter.
- helper-tool-read-file: Reads and returns the content of a file.
- tool-read-file: A tool that uses helper-tool-read-file to read the contents of a file specified by the file-path parameter.
- tool-search-web: A tool that uses tavily:websearch to perform a web search with the given query.
- helper-tool-summarize: Uses Gemini to summarize text.
- tool-summarize: Uses helper-tool-summarize to summarize text.

The following code listing provides a flexible framework for defining and using tools within the agent system. The define-tool macro and register-tool function allow for easy tool creation, while execute-tool handles their execution. The predefined tools demonstrate how to integrate file system operations, web search (via Tavily), and text summarization (using Gemini) into the agent's capabilities.


### agent-gemini.lisp

```lisp
;;; agent-gemini.lisp -- Gemini-based agent implementation
(in-package :cl-llm-agent)

;; Define a specialized agent using the Gemini LLM back-end
(defclass gemini-agent (base-agent) ()
  (:documentation "Agent using Gemini LLM back-end."))

(defmethod agent-llm-call ((agent gemini-agent) prompt)
  "Perform an LLM call for GEMINI-AGENT using gemini:generate."
  (declare (ignore agent))
  (gemini:generate prompt))
```

### agent-generic.lisp

```lisp
;;; agent-generic.lisp -- Core agent definitions for cl-llm-agent
(in-package :cl-llm-agent)

;; JSON parsing utilities
(defun parse-json (json-string)
  "Parse JSON-STRING into Lisp data; returns nil on failure."
  (ignore-errors
    (json:set-decoder-simple-list-semantics)
    (cl-json:decode-json-from-string json-string)))

(defun safe-parse-json (json-string)
  "Parse JSON-STRING, or signal an error if parsing fails."
  (or (parse-json json-string)
      (error "Failed to parse JSON: ~A" json-string)))

(defun strip-markdown-json (text)
  "Remove markdown fences around JSON in TEXT."  
  (let ((trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) text)))
    (if (and (search "```json" trimmed)
             (search "```" trimmed :start2 (1+ (search "```json" trimmed))))
        (subseq trimmed
                (search "```json" trimmed)
                (search "```" trimmed :start2 (1+ (search "```json" trimmed))))
        trimmed)))

;; Macro to define new agent classes
(defmacro define-agent (name &body body)
  "Define a new agent CLASS NAME with optional (:bases ...) and class BODY forms."
  (let ((bases nil)
        (forms body))
    (when (and body (consp (first body)) (eq (first (first body)) :bases))
      (setf bases (second (first body))
            forms (rest body)))
    `(defclass ,name ,bases
       ,@forms
       (:documentation ,(format nil "Agent class ~A" name)))))

;; Base agent class
(defclass base-agent ()
  ((name    :initarg :name    :accessor agent-name :initform nil)
   (context :initarg :context :accessor agent-context)
   (tools   :initform (make-hash-table :test #'equal) :accessor agent-tools))
  (:documentation "Base class for LLM agents."))

(defmethod initialize-instance :after ((agent base-agent) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp agent 'context)
    (setf (slot-value agent 'context) (make-context))))

;; LLM back-end generic
(defgeneric agent-llm-call (agent prompt)
  "Perform an LLM call with AGENT given PROMPT.")

(defun make-agent (agent-type &rest initargs &key context)
  "Construct an agent of AGENT-TYPE with INITARGS and optional CONTEXT."
  (apply #'make-instance agent-type
         (append (when context (list :context context))
                 initargs)))

(defparameter *agent-verbose* nil
  "When true, agent prints debug information.")

(defmethod agent-converse ((agent base-agent) user-input)
  "Perform one turn of conversation with AGENT given USER-INPUT."
  (when *agent-verbose*
    (format t "[agent-converse] input: ~A~%" user-input)
    (display-context (agent-context agent) "Context start:"))
  (let* ((tools-info (with-output-to-string (out)
                       (format out "tools:~%")
                       (dolist (t (list-tools))
                         (format out "  ~A: ~A~%"
                                 (getf t :name)
                                 (getf t :description)))))
         (prompt (format nil "~A~%User Input: ~A~%~%If using tools, respond in JSON." tools-info user-input))
         (raw (agent-llm-call agent prompt))
         (clean (strip-markdown-json raw))
         (parsed (safe-parse-json clean))
         (actions (if (getf parsed :actions)
                      (getf parsed :actions)
                      (list parsed))))
    (when *agent-verbose*
      (format t "[agent-converse] raw: ~A~%clean: ~A~%actions: ~A~%" raw clean actions))
    (loop with prev = nil
          for action across actions
          for name   = (getf action :action)
          for params = (getf action :parameters)
          do (setf prev (apply #'execute-tool name
                               (map 'list (lambda (p)
                                            (if (string-equal p "PREV_RESULT") prev p))
                                    params)))
          finally (return (or prev (format nil "~A" raw)))))
```

### context.lisp

```lisp
;;; context.lisp -- Context management for cl-llm-agent
(in-package :cl-llm-agent)

(defclass context ()
  ((data :initform (make-hash-table :test #'equal)
         :accessor context-data))
  (:documentation "Context class for storing key-value pairs."))

(defun make-context ()
  "Create a new agent context."
  (make-instance 'context))

(defgeneric context-set (ctx key value)
  "Set KEY to VALUE in CTX.")
(defmethod context-set ((ctx context) key value)
  (setf (gethash key (context-data ctx)) value))

(defgeneric context-get (ctx key)
  "Retrieve the value for KEY from CTX.")
(defmethod context-get ((ctx context) key)
  (gethash key (context-data ctx)))

(defgeneric context-remove (ctx key)
  "Remove KEY from CTX.")
(defmethod context-remove ((ctx context) key)
  (remhash key (context-data ctx)))

(defun display-context (ctx &optional (message "Context contents:"))
  "Pretty print the contents of CTX."  
  (format t "~A~%" message)
  (let ((table (context-data ctx)))
    (if (hash-table-p table)
        (maphash (lambda (k v)
                   (format t "  ~A: ~A~%" k v))
                 table)
        (format t "  Invalid context object~%"))))
```

### tools.lisp

```lisp
;;; tools.lisp -- Tool registry and predefined tools for cl-llm-agent
(in-package :cl-llm-agent)

(defvar *tool-registry* (make-hash-table :test #'equal)
  "Registry of registered tools.")

(defun register-tool (name &key description parameters parameter-example function)
  "Register a tool NAME with metadata and FUNCTION."
  (setf (gethash name *tool-registry*)
        (list :name name
              :description description
              :parameters parameters
              :parameter-example parameter-example
              :function function)))

(defun list-tools ()
  "Return a list of registered tools with metadata."
  (loop for data being the hash-value of *tool-registry*
        collect (list :name (getf data :name)
                      :description (getf data :description)
                      :parameters (getf data :parameters)
                      :parameter-example (getf data :parameter-example)
                      :function (getf data :function))))

(defun execute-tool (name &rest args)
  "Execute tool NAME with ARGS, checking parameter counts."
  (let ((data (gethash name *tool-registry*)))
    (unless data (error "Tool ~A not found" name))
    (let ((func   (getf data :function))
          (params (getf data :parameters)))
      (unless (= (length params) (length args))
        (error "Tool ~A expected ~A args but got ~A" name (length params) (length args)))
      (apply func args))))

(defmacro define-tool (name description parameters parameter-example function)
  "Convenience macro to register a tool."
  `(register-tool ,name
                  :description ,description
                  :parameters ',parameters
                  :parameter-example ,parameter-example
                  :function ,function))

;;; Predefined helper functions
(defun helper-read-directory (dir)
  "List files in DIR excluding hidden or backup files."
  (let ((path (truename (or dir "."))))
    (if (probe-file path)
        (remove-if (lambda (n)
                     (or (char= (char n 0) #\#)
                         (char= (char (aref n (1- (length n))) #\~)))
                   (uiop:directory-files path))
        (error "Directory not found: ~A" path))))

(register-tool "tool-read-directory"
               :description "Reads the contents of a directory."
               :parameters '(directory-path)
               :parameter-example "directory-path: string"
               :function #'helper-read-directory)

(defun helper-read-file (file)
  "Return the contents of FILE as a string, or error if missing."
  (if (probe-file file)
      (with-open-file (in file :direction :input)
        (with-output-to-string (out)
          (loop for line = (read-line in nil)
                while line do (write-line line out))))
      (error "File not found: ~A" file)))

(register-tool "tool-read-file"
               :description "Reads the contents of a file."
               :parameters '(file-path)
               :parameter-example "file-path: string"
               :function #'helper-read-file)

(register-tool "tool-search-web"
               :description "Search the web with Tavily."
               :parameters '(query)
               :parameter-example "query: string"
               :function (lambda (query) (tavily:websearch query)))

(defun helper-summarize (text)
  "Summarize TEXT using Gemini LLM backend."
  (let ((prompt (format nil "Summarize the following text:~%~A~%" text)))
    (gemini:generate prompt)))

(register-tool "tool-summarize"
               :description "Summarize text using Gemini."
               :parameters '(text)
               :parameter-example "text: string"
               :function #'helper-summarize)
```


## Code Discusion

These source files defines the core logic for creating and interacting with agents within the cl-llm-agent system. It includes mechanisms for managing agent contexts, defining agent types, handling conversations, and integrating with external tools (like those defined in tools.lisp). Here's a breakdown:

- Context Management:
  - context class: Represents a context for storing key-value pairs, implemented using a hash table.
  - make-context: Creates a new context instance.
  - context-set: Generic function to set a value in the context.
  - context-get: Generic function to get a value from the context.
  - context-remove: Generic function to remove a key from the context.
  - display-context: Pretty-prints the contents of a context object for debugging.
- Agent Definition:
  - define-agent: A macro for defining new agent types. It allows defining agents that inherit from other agent classes using the :bases keyword.
  - base-agent class: The base class for all agents. It has slots for:
    - tools: A hash table to store tools associated with the agent.
    - name: The name of the agent.
    - context: The agent's context (an instance of the context class).
- Agent Initialization:
  - initialize-instance :after method: Ensures that every base-agent has a context. If one isn't provided during initialization, it creates a default one.
  - make-agent: Creates an instance of an agent of a specified type. It allows passing a context, or creates a default one if not provided.
- Prompt Generation for Tools:
  - make-prompt-string: Creates a formatted string describing the available tools to the agent. This is used in the conversation prompt to inform the language model about the tools it can use.
- JSON Markdown Removal:
  - remove-json-markdown: Attempts to extract a JSON string from text that might contain markdown formatting. This is used to clean up responses from the language model that might be wrapped in markdown.
- Conversation Handling:
  - agent-converse: This method handles a single turn of conversation with the agent.
    - It constructs a prompt that includes the available tools and the user's input.
    - It calls agent-llm-call (which needs to be defined for specific agent types) to get a response from the language model.
    - It attempts to parse the response as JSON, looking for action and parameters keys that indicate a tool invocation. The response may contain either a single action or a list of actions.
    - It handles a list of actions, indicated by the key :ACTIONS. It iterates through the actions, executing each one in sequence.
    - If a parameter value is "PREV_RESULT", it's replaced with the result of the previous action.
    - It calls execute-tool to execute the specified tool with the extracted parameters.
    - If the response is not a tool invocation, it treats it as a direct response from the agent.
- Tool Function Retrieval:
  - get-tool-function: Retrieves the function associated with a given tool name, handling cases where the tool name might contain underscores or hyphens.
  - execute-tool Re-Implementation:
    - The code re-defines execute-tool (previously defined in tools.lisp) and calls get-tool-function to get and apply the tool's function.
- Concrete Agent Example (gemini-agent)
  - define-agent gemini-agent: Defines a specific agent type named gemini-agent that inherits from base-agent.
  - agent-llm-call: A function specialized for gemini-agent that calls gemini-generate-content to interact with the Gemini language model. For other agent types, it signals an error indicating that the LLM call is not implemented.

In essence, agent.lisp defines the core framework for agent interaction, conversation management, and tool integration. It provides a flexible structure for creating different types of agents that can interact with language models and utilize external tools based on the conversation context.

## test.lisp

```lisp
;;(ql:quickload :cl-llm-agent)

;; Create a context object
(defvar my-context (cl-llm-agent:make-context))

;; Set some initial data in the context
(setf (gethash "current-task" (cl-llm-agent:context-data my-context))
      "researching restaurants")
(setf (gethash "user-location" (cl-llm-agent:context-data my-context))
      "Paris")

;; Create a Gemini Agent and pass the context
(defvar my-agent (cl-llm-agent:make-agent 'cl-llm-agent::gemini-agent
                                          :context my-context))
;; try extracting context from agent
(defvar *context* (cl-llm-agent:agent-context my-agent))
(cl-llm-agent:display-context *context* "Context fetched from agent")
(cl-llm-agent:display-context my-context "Original context")

(cl-llm-agent:agent-converse
  my-agent
  "Search the web to find information on AI advancements.")

;; Agent interaction - the agent can now access and modify its context
(cl-llm-agent:agent-converse
  my-agent
  "Find restaurants based on my current task and location stored in the context.")

;; You can also access the context directly from outside the agent:
(format t "~%Current Task from Context: ~A~%"
        (gethash "current-task" (cl-llm-agent:context-data my-context)))

;; Example of setting context from outside:
(setf (gethash "user-cuisine-preference" (cl-llm-agent:context-data my-context))
      "Italian")

;; Next conversation turn - the agent can use the updated context
(cl-llm-agent:agent-converse my-agent "Now refine the restaurant search to Italian cuisine.")

(cl-llm-agent:agent-converse
  my-agent
  "What Lisp source files are in the current directory?")
 
(cl-llm-agent:agent-converse
  my-agent
  "Read the file 'README.md' in the current directory.")

(cl-llm-agent:agent-converse
  my-agent
  "Read the file 'test.txt' in the current directory and summarize the text in the file.")

(cl-llm-agent:agent-converse
  my-agent
  "Search the web to find information on AI advancements and then summarize the search results.")
```

The file test.lisp contains many small tests. In development I usually only run one or two tests at a time, commenting out the others. The test.lisp file demonstrates how to use the **cl-llm-agent** system. It's essentially a script that showcases the creation and interaction with a gemini-agent. Here's a concise description:

- Context Creation:
  - Creates a context object named my-context using cl-llm-agent:make-context.
  - Sets initial values in the context for "current-task" and "user-location".
- Agent Creation:
  - Creates an instance of gemini-agent named my-agent, passing the created my-context to it.
- Context Display:
  - Shows how to retrieve the context from the agent using cl-llm-agent:agent-context.
  - Uses cl-llm-agent:display-context to print the contents of the context, both the one retrieved from the agent and the original my-context.
- Agent Conversations:
  - Initiates a conversation with my-agent using cl-llm-agent:agent-converse.
  - The first conversation turn asks the agent to search for information on AI advancements, triggering a web search via Tavily.
  - The second turn instructs the agent to find restaurants based on the current-task and user-location stored in the context.
  - Accesses the context directly to retrieve the value of "current-task".
  - Sets a new value in the context for "user-cuisine-preference".
  - The next conversation turn asks the agent to refine the restaurant search based on the newly added cuisine preference.
  - Asks the agent to list Lisp source files in the current directory, invoking the tool-read-directory.
  - Asks the agent to read the file README.md, invoking tool-read-file.
  - Instructs the agent to read test.txt and summarize its content, utilizing both tool-read-file and tool-summarize.

In summary, test.lisp demonstrates a basic workflow for using the cl-llm-agent system: creating a context, initializing an agent with that context, and interacting with the agent through conversation turns. It showcases how the agent can leverage tools and context to perform tasks and respond to user requests. This script would be run after loading the other files in the project, allowing it to use the defined functions, classes, and tools.

## Example Output

Using some of the expressions in the file test.lisp (seen in the last listing), here are a few interactive examples in a LispWorks REPL (some output removed for brevity):

```text
$ lispworks
; Loading text file /Applications/LispWorks 8.0 (64-bit)/Library/lib/8-0-0-0/private-patches/load.lisp
LispWorks(R): The Common Lisp Programming Environment

CL-USER 1 > (ql:quickload :cl-llm-agent)
To load "cl-llm-agent":
  Load 1 ASDF system:
    cl-llm-agent
; Loading "cl-llm-agent"
.................................................
[package cl-llm-agent-gemini].To load "babel":
  Load 1 ASDF system:
    babel
; Loading "babel"

....................
[package jonathan-asd]To load "jonathan":
  Load 1 ASDF system:
    jonathan
; Loading "jonathan"
............................
[package cl-syntax-asd]...........................
[package trivial-types-asd].......................
[package cl-syntax-annot-asd].....................
[package cl-annot-asd]............................
[package proc-parse-asd]
To load "dexador":
  Load 1 ASDF system:
    dexador
; Loading "dexador"
..........................
[package fast-http-asd]...........................
[package cl-utilities-system].....................
[package xsubseq-asd].............................
[package smart-buffer-asd]........................
[package cl-cookie-asd]
; Loading "babel"
; Loading "jonathan"
; Loading "dexador"
(:CL-LLM-AGENT)

CL-USER 2 > (defvar my-context (cl-llm-agent:make-context))
MY-CONTEXT

CL-USER 3 > (defvar my-agent (cl-llm-agent:make-agent 'cl-llm-agent::gemini-agent
                                          :context my-context))
MY-AGENT

CL-USER 4 > (cl-llm-agent:agent-converse my-agent "Read the file 'test.txt' in the current directory and summarize the text in the file.")
&* * agent-converse: Read the file 'test.txt' in the current directory and summarize the text in the file.
Context at start of agent-converse call

LLM Response: ``json
{"actions": [{"action": "tool-read-file", "parameters": {"file_path": "test.txt"}}, {"action": "tool-summarize", "parameters": {"text": "PREV_RESULT"}}]}
``

Cleaned LLM Response: {"actions": [{"action": "tool-read-file", "parameters": {"file_path": "test.txt"}}, {"action": "tool-summarize", "parameters": {"text": "PREV_RESULT"}}]}
* agent-converse: action-request = ((ACTIONS ((ACTION . tool-read-file) (PARAMETERS (FILE--PATH . test.txt))) ((ACTION . tool-summarize) (PARAMETERS (TEXT . PREV_RESULT)))))

Executing tool tool-read-file with params (test.txt)
  tool-function: #<Function 1 subfunction of (TOP-LEVEL-FORM 12) 82200D1929>

Found tool function, calling it...parameters = (test.txt)
* tool-read-file in path test.txt
Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter. When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows, and is on average the third-brightest natural object in the night sky after the Moon and Venus.

Executing tool tool-summarize with params (Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter. When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows, and is on average the third-brightest natural object in the night sky after the Moon and Venus.
)
  tool-function: #<Function 1 subfunction of (TOP-LEVEL-FORM 15) 82200D1969>

Found tool function, calling it...parameters = (Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter. When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows, and is on average the third-brightest natural object in the night sky after the Moon and Venus.
)
* tool-summarize text: Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter. When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows, and is on average the third-brightest natural object in the night sky after the Moon and Venus.

* helper-tool-summarize: generated summary is:

Jupiter, the Solar System's largest planet and a gas giant, is easily visible from Earth and was known to ancient civilizations.  Its mass surpasses that of all other planets combined, and it's one of the brightest objects in the night sky.

Jupiter, the Solar System's largest planet and a gas giant, is easily visible from Earth and was known to ancient civilizations.  Its mass surpasses that of all other planets combined, and it's one of the brightest objects in the night sky.
"Tools executed. Final result: Jupiter, the Solar System's largest planet and a gas giant, is easily visible from Earth and was known to ancient civilizations.  Its mass surpasses that of all other planets combined, and it's one of the brightest objects in the night sky.
"
```

This last example used two tools, for reading the contents of a local file and for summarizing text.

In the next example we use the web search tool and the summarization tool (much of the output for the multiple web search results are not shown):

```text
CL-USER 5 > (cl-llm-agent:agent-converse my-agent "Search the web to find information on AI advancements and then summarize the search results.")
&* * agent-converse: Search the web to find information on AI advancements and then summarize the search results.
Context at start of agent-converse call

LLM Response: ``json
{"actions": [{"action": "tool_search_web", "parameters": {"param1": "AI advancements"}}, {"action": "tool_summarize", "parameters": {"param1": "PREV_RESULT"}}]}
``

Cleaned LLM Response: {"actions": [{"action": "tool_search_web", "parameters": {"param1": "AI advancements"}}, {"action": "tool_summarize", "parameters": {"param1": "PREV_RESULT"}}]}
* agent-converse: action-request = ((ACTIONS ((ACTION . tool_search_web) (PARAMETERS (PARAM-1 . AI advancements))) ((ACTION . tool_summarize) (PARAMETERS (PARAM-1 . PREV_RESULT)))))

Executing tool tool_search_web with params (AI advancements)
  tool-function: #<Function 1 subfunction of (TOP-LEVEL-FORM 13) 82200D18E9>

Found tool function, calling it...parameters = (AI advancements)
* tool-search-web query: AI advancements

* Calling tavily-search with qery: AI advancements
We furthered our industry-leading research in AI safety, developing new tools and techniques and integrating these advances into our latest models. We expanded SynthID’s capabilities to watermarking AI-generated text in the Gemini app and web experience, and video in Veo. To help increase overall transparency online, not just with content created by Google gen AI tools, we also joined the Coalition for Content Provenance and Authenticity (C2PA) as a steering committee member and collaborated on a new, more secure version of the technical standard, Content Credentials. Google Cloud #### New tools to help retailers build gen AI search and agents By Carrie Tharp Jan 12, 2025
By Sophia Velastegui: C200 member, Former Microsoft Chief AI Technology Officer and General Manager, AI Product; AI advisor for the National Science Foundation; formerly at tech giants Google/Alphabet & Apple; Board Director at Blackline (NASDAQ). Here are my top trends for the year and how they may predict our AI future: To realize the full ROI, companies are exploring more advanced AI solutions, customizing their tools to meet the unique needs of their business models. The potential for multimodal AI will shift how we communicate on and offline, enabling everyday users to create high-quality visuals but also threatening creative professionals and opening new doors for misinformation. A year feels like a lifetime when considering the rapid pace of AI advancements.
...

AI News AI Agents AI Productivity Tools Jenni AI Review Jasper AI Review AI for Thought AI Glossary AI Statistics AI How-To Guides What 2024 Taught Us About Ai  From smarter chatbots and lifelike voice clones to debates about job security and ethics, AI was at the center of it all. AI automation tools reshaped industries, especially in repetitive roles like data processing and customer service, leaving many worried about their future. Opportunities of AI in 2024 Enhanced productivity through smarter tools like agent AI and advanced chatbots. Top 10 AI Trends in 2024 AI saw major breakthroughs like advanced language models, realistic voice cloning, and agent AI capable of performing tasks independently. Did AI take over jobs in 2024? What is the future of AI in 2025?
Our four big bets for 2023 were that the next big thing in chatbots would be multimodal (check: the most powerful large language models out there, OpenAI’s GPT-4 and Google DeepMind’s Gemini, work with text, images and audio); that policymakers would draw up tough new regulations (check: Biden’s executive order came out in October and the European Union’s AI Act was 
...

 —Will Douglas Heaven
by Melissa Heikkilä & Will Douglas Heaven
Share
Popular
Deep Dive
Artificial intelligence
Google DeepMind used a large language model to solve an unsolved math problem
They had to throw away most of what it produced but there was gold among the garbage.

...
 
Robots that multitask
Inspired by some of the core techniques behind generative AI’s current boom, roboticists are starting to build more general-purpose robots that can do a wider range of tasks.


Executing tool tool_summarize with params (

 ... NOT SHOWN TEXT
 
)
  tool-function: #<Function 1 subfunction of (TOP-LEVEL-FORM 15) 82200D1969>

Found tool function, calling it...parameters = (

 ... NOT SHOWN TEXT

)
* tool-summarize text:

 ... NOT SHOWN TEXT
 
* helper-tool-summarize: generated summary is:

 ... NOT SHOWN TEXT
 
"Tools executed. Final result: Google advanced its AI safety research, expanding its SynthID watermarking technology to text and video.  Experts predict a rise in multimodal AI, impacting various industries and raising ethical concerns.  2024 saw significant AI advancements, including improved chatbots and automation tools, alongside debates about job displacement and the spread of misinformation.  Predictions for 2025 include continued AI development and increased focus on AI ethics.
"

CL-USER 6 > 
```

If you run this example yourself you will see that many pages of search results on AI advancements are summarized fairly well.

## Multi Agent Example Wrap Up

This example is currently a work in progress so please, dear reader, check the GitHub repository [https://github.com/mark-watson/cl-llm-agent](https://github.com/mark-watson/cl-llm-agent) for updates. I wrote this example for my own use. I want a simple framework that I can write many personalized agents for my own use. My end goal for this project is a single command line app agent tool that handles parts of my own work flow. You can clone this example and personalize it for your own use.

I would like to give **CREDIT** to two open source agent projects that provided ideas for my code featured in this chapter:

- AutoGen from Microsoft
- AgentWorkFlows from LlamaIndex

