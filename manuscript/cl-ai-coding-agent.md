# Building an AI Coding Assistant for Common Lisp

In the previous chapter we built a general-purpose multi-agent framework with a tool registry, context objects, and a JSON-based action protocol.

In this chapter we build `cl-ai-coding-agent`, a focused coding assistant that uses the `litelm` library from the previous `litelm` chapter. The `litelm` library speaks the OpenAI-compatible tool-calling protocol shared by many providers: we declare tools in a plain Lisp list format, the model returns typed tool-call requests, and our agent loop dispatches them directly. This eliminates the fragile JSON parsing layer entirely and lets the model decide autonomously when to inspect directories, read source files, write new files, or simply answer a question.

Because `litelm` routes calls by a `"provider/model"` string, the same agent code works with OpenAI, Gemini, Fireworks AI, DeepSeek, or a local Ollama server. The default model is the local Ollama model `qwen3.5:4b`, so the examples in this chapter run without any API key. Later in the Hacking the SBCL REPL chapter we integrate the agent further with reader macros (`#?`) and automatic error interception so the agent is always one keystroke away.

## Architecture

The agent follows a simple multi-round loop built on `litelm:completion`:

1. **Prompt augmentation**: The user's input is paired with a system message that describes the available tools. If the input looks like a stacktrace, additional diagnostic instructions are injected into the system message.
2. **Round 1**: The message list and tool declarations are sent to the model via `litelm:completion`. The model returns either a text response (no tools needed) or a list of tool-call requests.
3. **Tool dispatch**: Each tool call is executed locally. The agent can list directories, read files, and write files.
4. **Round 2+**: The assistant's tool calls and the tool results are appended to the message history, and the updated history is sent back to the model. The model may request more tool calls (for example, read a file after listing a directory) or return a final text response.
5. **Termination**: The loop repeats for up to 10 rounds, then returns whatever text the model has produced.

Here, the OpenAI-compatible tool-calling protocol in the library `litelm` handles the structure, so we never parse JSON tool invocations ourselves. The model's tool-call requests arrive as typed plists, and our tool results go back into the message history in the same Lisp format. This is both more reliable and simpler to implement.

## Project Structure

The project has three source files plus an ASDF system definition:

| File | Purpose |
|------|---------|
| `package.lisp` | Package definition and exports |
| `tools.lisp` | File-system tool implementations and litelm tool definitions |
| `agent.lisp` | System prompt construction, agent loop, and public API |
| `cl-ai-coding-agent.asd` | ASDF system definition |

The only external dependencies are the `litelm` library (developed in the litelm chapter, in the sibling directory `../litelm`) and UIOP (which ships with ASDF).

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
  :depends-on ("litelm" "uiop")
  :components ((:file "package")
               (:file "tools")
               (:file "agent")))
```

### package.lisp

The package exports six symbols: two query functions, an interactive REPL, a debug flag, and two configuration parameters:

```lisp
;;; package.lisp -- Package definition for cl-ai-coding-agent
(defpackage :cl-ai-coding-agent
  (:use :cl)
  (:export #:coding-agent-query
           #:coding-agent-query-file
           #:coding-agent-repl
           #:*verbose*
           #:*default-model*
           #:*max-tool-rounds*))
```

## File-System Tools

The agent has three tools, each consisting of a local implementation function and a corresponding litelm tool definition.

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

Each tool returns a string. This is important because tool results travel back to the model as text messages, so even `tool-list-directory` and `tool-write-file` produce human-readable string output rather than structured data.

The `tool-list-directory` function filters out hidden files (names starting with `.`), Emacs backup files (ending with `~`), and Emacs auto-save files (starting with `#`). The `enough-namestring` call strips the directory prefix so the output is clean relative names rather than full absolute paths.

The `tool-write-file` function calls `ensure-directories-exist` before writing, so the model can create files in new subdirectories without a separate `mkdir` step.

### litelm Tool Definitions

Each tool needs a corresponding definition that tells the model the function's name, description, and parameters. The `litelm` library uses a plain Lisp format: each tool is a list of `(name description ((param-name param-type param-description) ...))`.

```lisp
;;; ---- litelm tool definitions ----
;;;
;;; Tools use litelm's Lisp format:
;;;   (name description ((param type desc) ...))

(defun %make-tool-declarations ()
  "Build the list of litelm tool definitions
   for file-system tools."
  '((list_directory
     "List files and subdirectories in a directory.
      Returns one entry per line."
     ((path "string"
       "Absolute or relative directory path")))
    (read_file
     "Read the full contents of a text file and
      return it as a string."
     ((path "string"
       "Absolute or relative file path")))
    (write_file
     "Create or overwrite a file with the given
      content.  Parent directories are created
      automatically."
     ((path "string"
       "Absolute or relative file path")
      (content "string"
       "The full text content to write")))))
```

There is no JSON anywhere in our code: `litelm` translates these Lisp lists into the wire format expected by whichever provider the model string selects. Parameters are required by default, so the model knows it must supply both `path` and `content` when calling `write_file`.

### Tool Dispatch

When the model requests a tool call, `litelm` returns a plist with `:ID`, `:NAME`, and `:ARGUMENTS`. The dispatch function routes each call to the correct local function:

```lisp
;;; ---- Dispatch a tool-call plist ----

(defun dispatch-tool-call (fc)
  "Execute the tool described by tool-call plist
   FC (:ID :NAME :ARGUMENTS) as returned by
   litelm:response-tool-calls.
   Returns a string result."
  (let* ((name (getf fc :name))
         (args (or (getf fc :arguments)
                   (getf fc :args)))
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

The `:ARGUMENTS` value is a decoded `alist`, for example `((:path . "/tmp/test.lisp"))`, so the `get-arg` lambda uses `assoc` with `:test #'string-equal` to handle case variations. The outer `handler-case` catches tool errors (like missing files) and returns them as strings rather than signaling conditions that would break the agent loop. This is crucial: if a file does not exist, we want the model to see the error message and either try a different approach or explain the problem to the user.

## The Agent Core

### Stacktrace Detection

Before constructing the system message, the agent checks whether the user's input contains a stacktrace or error message. If it does, additional instructions are injected to guide the model toward root-cause analysis:

```lisp
;;; agent.lisp -- Core agent logic for cl-ai-coding-agent
(in-package :cl-ai-coding-agent)

(defvar *verbose* nil
  "When non-NIL, print debug information during
   agent execution.")

(defparameter *max-tool-rounds* 10
  "Maximum number of tool-use round-trips before
   the agent returns whatever it has.")

(defparameter *default-model* "ollama/qwen3.5:4b"
  "Default litelm model for the agent.
   Uses a local Ollama model, so no API key
   is needed.")

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

The patterns cover SBCL conditions (`UNDEFINED-FUNCTION`, `TYPE-ERROR`, `debugger invoked`), Python tracebacks (`Traceback (most recent call last)`), and Java or generic stack traces (`Exception in thread`, `Stack trace:`). The `char-equal` test makes the search case-insensitive. This detection is heuristic: it may occasionally fire on benign input containing the word "Error:", but false positives are harmless since the extra instructions only add diagnostic guidance without changing the agent's capabilities.

### System Message Construction

The system message establishes the agent's persona and informs it about available tools:

```lisp
;;; ---- System prompt construction ----

(defun %system-instructions (user-prompt)
  "Build the system message for USER-PROMPT."
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
use write_file, do not just print the code.

~A" stacktrace-instructions)))
```

Note that `%system-instructions` builds only the system message. The user's input travels as a separate `:user` entry in the message list, which matches the chat message format that `litelm `expects.

The system message explicitly instructs the model to use `write_file` when creating files rather than just printing code. Without this instruction, models tend to respond with code blocks in their text output, which is useful for a chatbot but unhelpful when you want the agent to actually create the file on disk.

The stacktrace instructions include a restraint: "Only use file-system tools if you genuinely need to see source code context." This prevents the model from reflexively reading every file in the project when the error message alone provides enough information for a diagnosis.

### The Agent Loop

The `coding-agent-query` function is the primary entry point. It maintains one message list and calls `litelm:completion` once per round:

```lisp
;;; ---- Agent loop ----

(defun coding-agent-query (prompt &key (model *default-model*))
  "Process PROMPT through the AI coding agent.
   The agent can read directories, read files,
   write new files, and diagnose stacktraces.
   MODEL is a litelm \"provider/model\" string,
   defaulting to *DEFAULT-MODEL*.
   Returns the final text response."
  (let* ((tools (%make-tool-declarations))
         (messages
          (list (list :system
                      (%system-instructions prompt))
                (list :user prompt)))
         (round 0)
         text calls)
    (when *verbose*
      (format t "~&[coding-agent] model: ~A~%~A~%"
              model messages))
    (loop
      (incf round)
      (let ((resp (litelm:completion model
                                     :messages messages
                                     :tools tools)))
        (setf text (litelm:response-content resp)
              calls (litelm:response-tool-calls resp))
        (when *verbose*
          (format t "[coding-agent] round-~D text: ~A~%"
                  round text)
          (format t "[coding-agent] round-~D calls: ~A~%"
                  round calls))
        (unless calls
          (return (or text "(no response from model)")))
        (when (> round *max-tool-rounds*)
          (return (or text "(agent exhausted tool rounds)")))
        (setf messages
              (nconc messages
                     (list (list :assistant text
                                 :tool-calls calls))
                     (mapcar (lambda (fc)
                               (let ((result
                                      (dispatch-tool-call fc)))
                                 (when *verbose*
                                   (format t
                                    "[coding-agent] tool ~A -> ~A~%"
                                    (getf fc :name)
                                    (subseq result 0
                                     (min 200
                                      (length result)))))
                                 (list :tool result
                                       :tool-call-id
                                       (getf fc :id))))
                             calls)))))))
```

The key design decisions here:

- **One growing message list**: Each round appends an `:assistant` message carrying the model's `:tool-calls`, followed by one `:tool` message per call carrying the result string and the `:tool-call-id`. Because `litelm` owns the wire translation, this same history works with every provider.
- **Early return**: If a round produces text with no tool calls, the agent returns immediately. Most simple questions ("What does `defmethod` do?") are answered in a single round.
- **Mapcar over calls**: All tool calls from a single round are executed before the next completion request. Models sometimes request multiple calls in one round (for example, reading two files at once), and processing them all at once is more efficient than sequential round-trips.
- **Round limit**: The `*max-tool-rounds*` parameter (default 10) prevents runaway loops. In practice, most interactions complete in 1 to 3 rounds: list directory, read file, respond.
- **Model selection**: The `:model` keyword accepts any litelm `"provider/model"` string, so switching from the local default to `gemini/gemini-2.5-flash` or `openai/gpt-4o` is a one-word change.
- **Verbose mode**: Setting `*verbose*` to `t` prints every message list, tool call, and response. The debug output truncates tool results to 200 characters to keep the output manageable.

### File-Based Queries

Stacktraces and error messages are often multi-line and contain double-quote characters, making them painful to paste into a Lisp string literal. The `coding-agent-query-file` function solves this by reading the prompt from a file:

```lisp
;;; ---- File-based query ----

(defun coding-agent-query-file (path
                                &optional prefix
                                (model *default-model*))
  "Read the contents of PATH and send them as a
   prompt to the coding agent.  This is the easiest
   way to diagnose multi-line stacktraces that may
   contain quote characters -- just save the error
   output to a file and pass the path here.
   PREFIX is an optional string prepended to the
   file contents (e.g. \"Fix this error:\").
   MODEL is a litelm \"provider/model\" string."
  (let* ((content (uiop:read-file-string path))
         (prompt (if prefix
                     (format nil "~A~%~A"
                             prefix content)
                     content)))
    (coding-agent-query prompt :model model)))
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

The `handler-case` around `coding-agent-query` ensures that API errors, network timeouts, and other failures produce a message rather than dropping into the SBCL debugger. This matters for a tool you use throughout the day, since you do not want to lose your REPL state because of a transient network issue.

## Installation

The agent depends on the litelm library, which lives in the sibling directory `../litelm`. Load both system definitions and quickload the agent:

```lisp
(asdf:load-asd "/path/to/litelm/litelm.asd")
(asdf:load-asd "/path/to/cl-ai-coding-agent/cl-ai-coding-agent.asd")
(ql:quickload :cl-ai-coding-agent)
```

The default model runs locally under Ollama, so no API key is needed. Pull the model once from your terminal:

```bash
ollama pull qwen3.5:4b
```

To use a hosted provider instead, pass a different `:model` string and set the corresponding API key environment variable (for example `GEMINI_API_KEY` for `gemini/` models), as described in the litelm chapter.

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

;; Use a hosted model instead of the local default
(cl-ai-coding-agent:coding-agent-query
  "What files are here?"
  :model "gemini/gemini-2.5-flash")
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
;; Prints model, tool calls, responses, and round info
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

The following session demonstrates the agent's multi-round tool use. The user asks the agent to describe the Lisp files in a directory, and the agent lists the directory, reads each file, then summarizes them:

```text
* (cl-ai-coding-agent:coding-agent-query
    "List the .lisp files in this directory
     and describe what each one does")

The current directory contains three Lisp source files:

1. **package.lisp** - Defines the `cl-ai-coding-agent`
   package and exports six symbols: `coding-agent-query`,
   `coding-agent-query-file`, `coding-agent-repl`,
   `*verbose*`, `*default-model*`, and `*max-tool-rounds*`.

2. **tools.lisp** - Implements three file-system tools
   (`list_directory`, `read_file`, `write_file`) that
   the agent can use autonomously, plus the litelm tool
   definitions that describe these tools to the model.

3. **agent.lisp** - Contains the core agent loop, system
   message construction, stacktrace detection, and the
   public API functions.
```

Behind the scenes, this query triggered three tool-use rounds:

1. **Round 1**: The model called `list_directory` with path `"."` and received the file listing.
2. **Round 2**: The model called `read_file` three times (once per `.lisp` file) to read their contents.
3. **Round 3**: The model produced the final text summary.


## Key Takeaways

1. **Native tool calling over JSON parsing**: `litelm` implements the OpenAI-compatible tool-calling protocol shared by many providers. By declaring tools as plain Lisp lists and dispatching the model's typed plist requests directly, we avoid the brittleness of parsing free-form JSON from model output.

2. **Provider independence**: Because the agent loop only calls `litelm:completion`, the entire library works unchanged against OpenAI, Gemini, Fireworks AI, DeepSeek, or a local Ollama server. The default `ollama/qwen3.5:4b` model needs no API key.

3. **Returning errors as strings**: Wrapping tool dispatch in `handler-case` and returning error messages as strings (rather than signaling conditions) keeps the agent loop running. The model can see the error and adapt, for example by trying a different file path.

4. **Stacktrace-aware prompting**: Detecting error patterns in the input and injecting diagnostic instructions produces better root-cause analysis. The restraint instruction ("only use tools if you genuinely need context") prevents unnecessary file reads.

5. **File-based input for awkward text**: The `coding-agent-query-file` function sidesteps the quoting problem inherent in pasting multi-line, quote-heavy stacktraces into Lisp string literals.

6. **Composition with the REPL**: This library is designed to be loaded into `~/.sbclrc` and used alongside normal Lisp development. The Hacking the SBCL REPL chapter shows how to integrate it with `#?` reader macros and automatic error interception for a seamless coding experience.

## Wrap Up for cl-llm-agent

You can use this example in building your own coding environment. In the chapter **Hacking the SBCL REPL** we'll see how to use this agent in an interactive SBCL REPL.

## Optional Practice Problems

1. **Add a `grep_files` Tool:** The agent can list directories and read files, but it has no way to search for text across multiple files. Add a fourth tool `grep_files` that accepts a `pattern` string and a `directory` string, uses `uiop:run-program` to call the system `grep` command (e.g., `grep -rnl pattern directory`), and returns matching filenames and line numbers as a string. Write the corresponding litelm tool definition in `%make-tool-declarations`, add a dispatch clause in `dispatch-tool-call`, and test it by asking the agent "Find all files that reference `defmethod` in this directory."

2. **Extend Stacktrace Detection:** The current `stacktrace-p` function uses simple substring matching via `search`. Extend it to also recognize Clojure stacktraces (patterns like `clojure.lang.ExceptionInfo`, `at clojure.core`), Rust panics (`thread 'main' panicked at`), and Go panics (`goroutine 1 [running]`). Then write a function `classify-error-type` that returns a keyword (`:sbcl`, `:python`, `:java`, `:clojure`, `:rust`, `:go`, or `:unknown`) based on which patterns match, and modify `%system-instructions` to inject language-specific diagnostic instructions depending on the classification.

3. **Conversation Logger:** The agent currently discards all intermediate tool calls and responses after the session ends. Write a logging layer that wraps `coding-agent-query`: before each query, open (or create) a log file `agent-log.jsonl` and append a JSON object for each event: the initial prompt, each tool call (name and arguments), each tool response, and the final answer. Use `litelm:json-encode` to serialize each entry. This provides an audit trail and lets you replay sessions later.

4. **Safety Sandbox:** The `tool-write-file` function can write to any path, including system files. Write a wrapper function `sandboxed-write-file` that accepts a list of allowed directory prefixes (e.g., `("/tmp/" "/home/user/projects/")`) and checks that the resolved absolute path of the target file starts with one of the allowed prefixes before writing. If the path is outside the sandbox, return an error string instead of writing. Integrate this into `dispatch-tool-call` so that write operations are always sandboxed. Discuss why `probe-file` alone is insufficient for this check (hint: symlinks).

5. **Batch File Reviewer:** Write a function `review-project` that takes a directory path, uses `tool-list-directory` to discover all `.lisp` files, then calls `coding-agent-query` once with a prompt that includes the contents of every file (read via `tool-read-file`) and asks the agent to review the code for style issues, potential bugs, and missing documentation. Be mindful of context window limits: estimate the prompt size before sending (a rough heuristic is four characters per token), and if it exceeds a threshold (e.g., 100,000 estimated tokens), split the files into batches and make multiple queries.

6. **Token-Aware Agent Loop:** The current agent loop has a round limit (`*max-tool-rounds*`) but no token budget. Modify `coding-agent-query` to accept an optional `:max-tokens` keyword parameter. After each call to `litelm:completion`, inspect `litelm:response-usage`, which returns a plist `(:prompt-tokens n :completion-tokens n :total-tokens n)`, and check the `:total-tokens` value against `:max-tokens`. If the budget is exceeded, return early with a note that the token budget ran out, or truncate the oldest tool responses from the message history and continue. This teaches you about managing context windows in multi-round LLM interactions.
