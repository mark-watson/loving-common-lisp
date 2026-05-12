# Building an AI Coding Assistant for Common Lisp

In the previous chapter on Agents Orchestrating LLM Tool Use we built a general-purpose multi-agent framework with a tool registry, context objects, and a JSON-based action protocol. That framework is flexible, but its generality comes at a cost: the model must produce well-formed JSON action lists, the tool registry uses string-keyed hash tables, and the conversation loop parses free-form LLM output that may or may not contain markdown fences. For an agent whose only job is to help you write and debug code from within the SBCL REPL, we can do better.

In this chapter we build `cl-ai-coding-agent`, a focused coding assistant that uses Gemini's native function-calling API — the Interactions API — instead of asking the model to emit JSON tool invocations. The model declares which functions it wants to call using a structured protocol, the Interactions API returns typed function-call objects, and our agent loop dispatches them directly. This eliminates the fragile JSON parsing layer entirely and lets Gemini decide autonomously when to inspect directories, read source files, write new files, or simply answer a question.

The result is a small library — three source files totaling roughly 300 lines — that you can load into any SBCL session and use immediately. Later in the Hacking the SBCL REPL chapter we integrate it further with reader macros (`#?`) and automatic error interception so the agent is always one keystroke away.

## Architecture

The agent follows a simple multi-turn loop built on the Gemini Interactions API:

1. **Prompt augmentation** — The user's input is wrapped with a system prompt that describes the available tools. If the input looks like a stacktrace, additional diagnostic instructions are injected.
2. **Turn 1** — The augmented prompt and tool declarations are sent to Gemini via `generate-with-tools`. Gemini returns either a text response (no tools needed) or a list of function-call requests.
3. **Tool dispatch** — Each function call is executed locally. The agent can list directories, read files, and write files.
4. **Turn 2+** — Tool results are sent back via `continue-with-function-responses`. Gemini may request more tool calls (e.g., read a file after listing a directory) or return a final text response.
5. **Termination** — The loop repeats for up to 10 rounds, then returns whatever text the model has produced.

This is fundamentally different from the `cl-llm-agent` framework in the previous chapter. There, the model had to produce a JSON `{"actions": [...]}` response that our code parsed and dispatched. Here, the Interactions API handles the structured protocol — we never parse JSON tool invocations ourselves. The model's function-call requests arrive as typed plists, and our tool results are sent back through the same API. This is both more reliable and simpler to implement.

## Project Structure

The project has three source files plus an ASDF system definition:

| File | Purpose |
|------|---------|
| `package.lisp` | Package definition and exports |
| `tools.lisp` | File-system tool implementations and Gemini function declarations |
| `agent.lisp` | System prompt construction, agent loop, and public API |
| `cl-ai-coding-agent.asd` | ASDF system definition |

The only external dependency beyond SBCL itself is the `gemini` library (developed in the Gemini chapter) and UIOP (which ships with ASDF).

### cl-ai-coding-agent.asd

```lisp
;;; cl-ai-coding-agent.asd -- ASDF system definition
(in-package #:asdf-user)

(defsystem "cl-ai-coding-agent"
  :name "cl-ai-coding-agent"
  :version "0.1.0"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :description
  "An AI coding agent that reads directories and files,
   writes new files, and diagnoses stacktraces."
  :depends-on ("gemini" "uiop")
  :components ((:file "package")
               (:file "tools")
               (:file "agent")))
```

### package.lisp

The package exports four symbols — two query functions, an interactive REPL, and a debug flag:

```lisp
;;; package.lisp -- Package definition for cl-ai-coding-agent
(defpackage :cl-ai-coding-agent
  (:use :cl)
  (:export #:coding-agent-query
           #:coding-agent-query-file
           #:coding-agent-repl
           #:*verbose*))
```

## File-System Tools

The agent has three tools, each consisting of a local implementation function and a corresponding Gemini function declaration.

### Tool Implementations

```lisp
;;; tools.lisp -- File-system tools for cl-ai-coding-agent
(in-package :cl-ai-coding-agent)

;;; ---- Helper functions executed locally ----

(defun tool-list-directory (dir)
  "List files and subdirectories in DIR.
   Excludes hidden and backup entries.
   Returns a newline-separated string of pathnames."
  (let* ((resolved (uiop:ensure-directory-pathname
                    (or dir ".")))
         (entries
          (append (uiop:directory-files resolved)
                  (uiop:subdirectories resolved))))
    (if entries
        (with-output-to-string (out)
          (dolist (e entries)
            (let ((name (enough-namestring e resolved)))
              (unless (or (uiop:string-prefix-p "." name)
                          (uiop:string-suffix-p "~" name)
                          (uiop:string-prefix-p "#" name))
                (format out "~A~%" name)))))
        (format nil "(empty directory: ~A)" resolved))))

(defun tool-read-file (path)
  "Return the contents of PATH as a string.
   Signals an error when the file does not exist."
  (let ((truepath (probe-file path)))
    (unless truepath
      (error "File not found: ~A" path))
    (uiop:read-file-string truepath)))

(defun tool-write-file (path content)
  "Write CONTENT to PATH, creating parent directories
   as needed.  Returns a confirmation message."
  (let ((pathname (pathname path)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (write-string content out))
    (format nil "Wrote ~D characters to ~A"
            (length content) path)))
```

Each tool returns a string. This is important — the Gemini Interactions API expects function results to be text, so even `tool-list-directory` and `tool-write-file` produce human-readable string output rather than structured data.

The `tool-list-directory` function filters out hidden files (names starting with `.`), Emacs backup files (ending with `~`), and Emacs auto-save files (starting with `#`). The `enough-namestring` call strips the directory prefix so the output is clean relative names rather than full absolute paths.

The `tool-write-file` function calls `ensure-directories-exist` before writing, so the model can create files in new subdirectories without a separate `mkdir` step.

### Gemini Function Declarations

Each tool needs a corresponding declaration that tells Gemini the function's name, description, parameter types, and which parameters are required:

```lisp
;;; ---- Gemini function declarations ----

(defun %make-tool-declarations ()
  "Build the list of Gemini function declarations
   for file-system tools."
  (list
   (gemini:make-function-declaration
    "list_directory"
    "List files and subdirectories in a directory.
     Returns one entry per line."
    '(("path" "STRING"
       "Absolute or relative directory path"))
    '("path"))

   (gemini:make-function-declaration
    "read_file"
    "Read the full contents of a text file and
     return it as a string."
    '(("path" "STRING"
       "Absolute or relative file path"))
    '("path"))

   (gemini:make-function-declaration
    "write_file"
    "Create or overwrite a file with the given
     content.  Parent directories are created
     automatically."
    '(("path" "STRING"
       "Absolute or relative file path")
      ("content" "STRING"
       "The full text content to write"))
    '("path" "content"))))
```

Each declaration is a hash-table built by `gemini:make-function-declaration`. The third argument is a list of `(name type description)` triples defining the parameters, and the optional fourth argument lists which parameters are required. These declarations are sent to Gemini alongside the user's prompt so the model knows exactly what tools are available and how to call them.

### Tool Dispatch

When Gemini requests a function call, it returns a plist with `:NAME`, `:ID`, and `:ARGS`. The dispatch function routes each call to the correct local function:

```lisp
;;; ---- Dispatch a function-call plist ----

(defun dispatch-tool-call (fc)
  "Execute the tool described by function-call
   plist FC (:NAME :ID :ARGS).
   Returns a string result."
  (let* ((name (getf fc :name))
         (args (getf fc :args))
         (get-arg (lambda (key)
                    (cdr (assoc key args
                                :test #'string-equal)))))
    (handler-case
        (cond
          ((string-equal name "list_directory")
           (tool-list-directory
            (funcall get-arg "path")))
          ((string-equal name "read_file")
           (tool-read-file
            (funcall get-arg "path")))
          ((string-equal name "write_file")
           (tool-write-file
            (funcall get-arg "path")
            (funcall get-arg "content")))
          (t (format nil "Unknown tool: ~A" name)))
      (error (e)
        (format nil "Tool error (~A): ~A" name e)))))
```

The `:ARGS` value is a `cl-json`-decoded alist — for example, `((:PATH . "/tmp/test.lisp"))` — so the `get-arg` lambda uses `assoc` with `:test #'string-equal` to handle case variations. The outer `handler-case` catches tool errors (like missing files) and returns them as strings rather than signaling conditions that would break the agent loop. This is crucial: if a file doesn't exist, we want Gemini to see the error message and either try a different approach or explain the problem to the user.

## The Agent Core

### Stacktrace Detection

Before constructing the system prompt, the agent checks whether the user's input contains a stacktrace or error message. If it does, additional instructions are injected to guide the model toward root-cause analysis:

```lisp
;;; agent.lisp -- Core agent logic for cl-ai-coding-agent
(in-package :cl-ai-coding-agent)

(defvar *verbose* nil
  "When non-NIL, print debug information during
   agent execution.")

(defparameter *max-tool-rounds* 10
  "Maximum number of tool-use round-trips before
   the agent returns whatever it has.")

;;; ---- Stacktrace detection ----

(defparameter *stacktrace-patterns*
  '("Backtrace"
    "BACKTRACE"
    "debugger invoked"
    "Unhandled"
    "HANDLER-BIND"
    "The value"
    "is not of type"
    "UNDEFINED-FUNCTION"
    "SIMPLE-ERROR"
    "PROGRAM-ERROR"
    "TYPE-ERROR"
    "UNBOUND-VARIABLE"
    "SB-INT:SIMPLE-READER-ERROR"
    "Traceback (most recent call last)"
    "at .* line [0-9]+"
    "Exception in thread"
    "Error:"
    "Stack trace:")
  "Patterns indicating the input contains a
   stacktrace or error message.")

(defun stacktrace-p (text)
  "Return T if TEXT likely contains a stacktrace
   or Common Lisp error output."
  (some (lambda (pat)
          (search pat text :test #'char-equal))
        *stacktrace-patterns*))
```

The patterns cover SBCL conditions (`UNDEFINED-FUNCTION`, `TYPE-ERROR`, `debugger invoked`), Python tracebacks (`Traceback (most recent call last)`), and Java/generic stack traces (`Exception in thread`, `Stack trace:`). The `char-equal` test makes the search case-insensitive. This detection is heuristic — it may occasionally fire on benign input containing the word "Error:" — but false positives are harmless since the extra instructions only add diagnostic guidance without changing the agent's capabilities.

### System Prompt Construction

The system prompt establishes the agent's persona and informs it about available tools:

```lisp
;;; ---- System prompt construction ----

(defun %system-prompt (user-prompt)
  "Build the full prompt sent to Gemini, including
   the system instructions and the user's input."
  (let ((stacktrace-instructions
         (if (stacktrace-p user-prompt)
             "The user's input contains a stacktrace
or error message.  Analyze it carefully:
1. Identify the root cause of the error.
2. Explain what went wrong in plain English.
3. Suggest a concrete fix with corrected code.
Only use file-system tools if you genuinely need
to see source code context.  If the error message
is self-explanatory, answer directly without
reading files.

"
             "")))
    (format nil
"You are an expert Common Lisp coding assistant.
You have access to three file-system tools:
  - list_directory: list contents of a directory
  - read_file: read a file's contents
  - write_file: create or overwrite a file

Use these tools when the user asks you to inspect
or modify files.  When creating new files, always
use write_file — do NOT just print the code.

~AUser request:
~A" stacktrace-instructions user-prompt)))
```

The system prompt explicitly instructs the model to use `write_file` when creating files rather than just printing code. Without this instruction, models tend to respond with code blocks in their text output — useful for a chatbot, but unhelpful when you want the agent to actually create the file on disk.

The stacktrace instructions include a restraint: "Only use file-system tools if you genuinely need to see source code context." This prevents the model from reflexively reading every file in the project when the error message alone provides enough information for a diagnosis.

### The Agent Loop

The `coding-agent-query` function is the primary entry point. It orchestrates the multi-turn conversation with Gemini:

```lisp
;;; ---- Agent loop ----

(defun coding-agent-query (prompt)
  "Process PROMPT through the AI coding agent.
   The agent can read directories, read files,
   write new files, and diagnose stacktraces.
   Returns the final text response."
  (let* ((declarations (%make-tool-declarations))
         (full-prompt  (%system-prompt prompt)))
    (when *verbose*
      (format t "~&[coding-agent] prompt:~%~A~%"
              full-prompt))
    ;; Turn 1 -- send prompt with tools
    (multiple-value-bind (text calls interaction-id)
        (gemini:generate-with-tools
         full-prompt declarations)
      (when *verbose*
        (format t "[coding-agent] turn-1 text: ~A~%"
                text)
        (format t "[coding-agent] turn-1 calls: ~A~%"
                calls))
      ;; If no tool calls, return the text directly
      (unless calls
        (return-from coding-agent-query
          (or text "(no response from model)")))
      ;; Multi-turn tool loop
      (loop for round from 1 to *max-tool-rounds*
            while calls
            do
         (let ((responses
                (mapcar
                 (lambda (fc)
                   (let ((result
                          (dispatch-tool-call fc)))
                     (when *verbose*
                       (format t
                        "[coding-agent] tool ~A -> ~A~%"
                        (getf fc :name)
                        (subseq result 0
                         (min 200
                          (length result)))))
                     (list :name (getf fc :name)
                           :id   (getf fc :id)
                           :response result)))
                 calls)))
           (multiple-value-bind
               (next-text next-calls next-iid)
               (gemini:continue-with-function-responses
                interaction-id
                responses
                declarations)
             (when *verbose*
               (format t
                "[coding-agent] round-~D text: ~A~%"
                round next-text)
               (format t
                "[coding-agent] round-~D calls: ~A~%"
                round next-calls))
             (setf text           next-text
                   calls          next-calls
                   interaction-id next-iid)))
         finally
         (return
          (or text
              "(agent exhausted tool rounds)"))))))
```

The key design decisions here:

- **Early return** — If Turn 1 produces text with no function calls, the agent returns immediately. Most simple questions ("What does `defmethod` do?") are answered in a single turn.
- **Mapcar over calls** — All function calls from a single turn are executed before sending results back. Gemini sometimes requests multiple calls in one turn (e.g., listing two directories simultaneously), and processing them all at once is more efficient than sequential round-trips.
- **Round limit** — The `*max-tool-rounds*` parameter (default 10) prevents runaway loops. In practice, most interactions complete in 1–3 rounds: list directory → read file → respond.
- **Verbose mode** — Setting `*verbose*` to `t` prints every prompt, tool call, and response. The debug output truncates tool results to 200 characters to keep the output manageable.

### File-Based Queries

Stacktraces and error messages are often multi-line and contain double-quote characters, making them painful to paste into a Lisp string literal. The `coding-agent-query-file` function solves this by reading the prompt from a file:

```lisp
;;; ---- File-based query ----

(defun coding-agent-query-file (path
                                &optional prefix)
  "Read the contents of PATH and send them as a
   prompt to the coding agent.  This is the easiest
   way to diagnose multi-line stacktraces that may
   contain quote characters -- just save the error
   output to a file and pass the path here.
   PREFIX is an optional string prepended to the
   file contents (e.g. \"Fix this error:\")."
  (let* ((content (uiop:read-file-string path))
         (prompt (if prefix
                     (format nil "~A~%~A"
                             prefix content)
                     content)))
    (coding-agent-query prompt)))
```

The typical workflow: copy a stacktrace to the clipboard, save it to a file (`pbpaste > /tmp/error.txt` on macOS), and call `(coding-agent-query-file "/tmp/error.txt")`.

### Interactive REPL

For extended sessions, the agent provides its own REPL loop:

```lisp
;;; ---- Interactive REPL ----

(defun coding-agent-repl ()
  "Start an interactive REPL for the coding agent.
   Type 'quit' or 'exit' to leave."
  (format t "~&AI Coding Agent (type quit to exit)~%")
  (loop
    (format t "~&> ")
    (finish-output)
    (let ((input (read-line *standard-input*
                            nil nil)))
      (when (or (null input)
                (string-equal (string-trim
                               '(#\Space) input)
                              "quit")
                (string-equal (string-trim
                               '(#\Space) input)
                              "exit"))
        (format t "~&Goodbye.~%")
        (return))
      (let ((trimmed (string-trim '(#\Space) input)))
        (when (plusp (length trimmed))
          (let ((response
                 (handler-case
                     (coding-agent-query trimmed)
                   (error (e)
                     (format nil "Error: ~A" e)))))
            (format t "~&~A~%" response)))))))
```

The `handler-case` around `coding-agent-query` ensures that API errors, network timeouts, and other failures produce a message rather than dropping into the SBCL debugger. This matters for a tool you use throughout the day — you don't want to lose your REPL state because of a transient network issue.

## Installation

Clone or symlink the project into your Quicklisp local-projects directory:

```bash
# From the book's repository
ln -s $(pwd)/src/cl-ai-coding-agent \
      ~/quicklisp/local-projects/cl-ai-coding-agent

# Also need the gemini library
ln -s $(pwd)/src/gemini \
      ~/quicklisp/local-projects/gemini
```

Then in SBCL:

```lisp
(ql:quickload :cl-ai-coding-agent)
```

You need a `GOOGLE_API_KEY` environment variable set for the Gemini API.

## Usage Examples

### One-Shot Queries

```lisp
;; Ask about files in the current directory
(cl-ai-coding-agent:coding-agent-query
  "What files are in the current directory?")

;; Generate and write a new file
(cl-ai-coding-agent:coding-agent-query
  "Write a file hello.lisp with a hello-world function.")

;; Diagnose an error
(cl-ai-coding-agent:coding-agent-query
  "debugger invoked on a UNDEFINED-FUNCTION:
   The function FOO is undefined.")
```

### Diagnosing Stacktraces from Files

```lisp
;; Save a stacktrace to a file:
;;   pbpaste > /tmp/error.txt
;;
;; Then in the REPL:
(cl-ai-coding-agent:coding-agent-query-file
  "/tmp/error.txt")

;; With a context prefix:
(cl-ai-coding-agent:coding-agent-query-file
  "/tmp/error.txt"
  "Fix this error in my project:")
```

### Debug Mode

```lisp
(setf cl-ai-coding-agent:*verbose* t)
(cl-ai-coding-agent:coding-agent-query
  "List the Lisp files in src/")
;; Prints tool calls, responses, and round info
```

### Interactive REPL

```lisp
(cl-ai-coding-agent:coding-agent-repl)
;; AI Coding Agent (type quit to exit)
;; > What files are here?
;; ...
;; > Write a test file for the cache-engine
;; ...
;; > quit
```

## Example Session

The following session demonstrates the agent's multi-turn tool use. The user asks the agent to describe the Lisp files in a directory — the agent lists the directory, reads each file, then summarizes them:

```text
* (cl-ai-coding-agent:coding-agent-query
    "List the .lisp files in this directory
     and describe what each one does")

The current directory contains three Lisp source files:

1. **package.lisp** — Defines the `cl-ai-coding-agent`
   package and exports four symbols: `coding-agent-query`,
   `coding-agent-query-file`, `coding-agent-repl`, and
   `*verbose*`.

2. **tools.lisp** — Implements three file-system tools
   (`list_directory`, `read_file`, `write_file`) that
   the agent can use autonomously, plus the Gemini
   function declarations that describe these tools to
   the model.

3. **agent.lisp** — Contains the core agent loop, system
   prompt construction, stacktrace detection, and the
   public API functions.
```

Behind the scenes, this query triggered three tool-use rounds:

1. **Round 1**: Gemini called `list_directory` with path `"."` and received the file listing.
2. **Round 2**: Gemini called `read_file` three times (once per `.lisp` file) to read their contents.
3. **Round 3**: Gemini produced the final text summary.

## Comparison with cl-llm-agent

The `cl-ai-coding-agent` and the `cl-llm-agent` framework from the previous chapter solve the same problem — giving an LLM access to external tools — but they make fundamentally different engineering choices:

| Aspect | cl-llm-agent | cl-ai-coding-agent |
|--------|-------------|-------------------|
| Tool protocol | LLM generates JSON; client parses it | Gemini Interactions API; structured function calls |
| Error handling | JSON parse failures → crashes | API-level; tools return error strings |
| Tool registry | Dynamic hash-table with `register-tool` | Static; three built-in tools |
| Multi-step | `PREV_RESULT` placeholder in JSON | Native multi-turn via `interaction-id` |
| Dependencies | cl-json, gemini, tavily, fiveam | gemini, uiop |
| Scope | General-purpose framework | Focused coding assistant |

The `cl-llm-agent` framework is more extensible — you can register arbitrary tools at runtime, compose agents, and use different LLM backends. The `cl-ai-coding-agent` is more reliable for its specific use case because it eliminates the JSON parsing layer entirely. Neither approach is universally better; they serve different design goals.

## Key Takeaways

1. **Native function calling over JSON parsing** — The Gemini Interactions API provides a structured protocol for tool use. By declaring functions with `make-function-declaration` and dispatching the model's typed requests directly, we avoid the brittleness of parsing free-form JSON from model output.

2. **Returning errors as strings** — Wrapping tool dispatch in `handler-case` and returning error messages as strings (rather than signaling conditions) keeps the agent loop running. The model can see the error and adapt — for example, trying a different file path.

3. **Stacktrace-aware prompting** — Detecting error patterns in the input and injecting diagnostic instructions produces better root-cause analysis. The restraint instruction ("only use tools if you genuinely need context") prevents unnecessary file reads.

4. **File-based input for awkward text** — The `coding-agent-query-file` function sidesteps the quoting problem inherent in pasting multi-line, quote-heavy stacktraces into Lisp string literals.

5. **Composition with the REPL** — This library is designed to be loaded into `~/.sbclrc` and used alongside normal Lisp development. The Hacking the SBCL REPL chapter shows how to integrate it with `#?` reader macros and automatic error interception for a seamless coding experience.

## Wrap Up for cl-llm-agent

You can use this example in building your own coding environment. In the chapter **Hacking the SBCL REPL** we'll see how to use this agent in an interactive SBCL REPL.
