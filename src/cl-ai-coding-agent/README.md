# cl-ai-coding-agent

A Common Lisp AI coding agent powered by the Gemini API.  The agent
takes a string prompt as input and can autonomously read directories,
read files, write new files, and diagnose stacktraces/error messages.

## Features

- **Directory inspection** — list files and subdirectories
- **File reading** — read source files for context
- **File writing** — create or overwrite files with generated code
- **Stacktrace diagnosis** — automatically detects error messages
  and provides root-cause analysis with fix suggestions
- **Multi-round tool use** — the agent can chain multiple
  tool calls (e.g., list dir → read file → respond)

## Requirements

- SBCL with Quicklisp
- `GOOGLE_API_KEY` environment variable set
- The `gemini` package (from this repository)

## Installation

```bash
export GOOGLE_API_KEY="your-key-here"
```

In your Lisp REPL:

```lisp
(ql:quickload :cl-ai-coding-agent)
```

## Usage

### One-shot query

```lisp
;; Ask about files in the current directory
(cl-ai-coding-agent:coding-agent-query
  "What files are in the current directory?")

;; Diagnose a stacktrace
(cl-ai-coding-agent:coding-agent-query
  "debugger invoked on a UNDEFINED-FUNCTION:
   The function FOO is undefined.")

;; Write a new file
(cl-ai-coding-agent:coding-agent-query
  "Write a file hello.lisp with a hello-world function.")
```

### Interactive REPL

```lisp
(cl-ai-coding-agent:coding-agent-repl)
;; AI Coding Agent (type quit to exit)
;; > What files are here?
;; ...
;; > quit
```

### Diagnose a stacktrace from a file

Stacktraces often contain quote characters and span
multiple lines, making them awkward to paste into a
Lisp string.  Save the error output to a file instead:

```bash
# In your terminal, copy the stacktrace to a file:
pbpaste > /tmp/error.txt
```

```lisp
;; Then in SBCL:
(cl-ai-coding-agent:coding-agent-query-file
  "/tmp/error.txt")

;; Or with a prefix prompt:
(cl-ai-coding-agent:coding-agent-query-file
  "/tmp/error.txt"
  "Fix this error in my project:")
```

### Debug mode

```lisp
(setf cl-ai-coding-agent:*verbose* t)
(cl-ai-coding-agent:coding-agent-query "...")
;; Prints tool calls, responses, and round info
```

## Exported API

| Symbol | Type | Description |
|--------|------|-------------|
| `coding-agent-query` | Function | `(prompt) -> string` -- one-shot agent query |
| `coding-agent-query-file` | Function | `(path &optional prefix) -> string` -- query from file |
| `coding-agent-repl` | Function | `() -> nil` -- interactive REPL loop |
| `*verbose*` | Variable | When non-NIL, prints debug info |

## Architecture

The agent uses Gemini's Interactions API with function calling:

1. User prompt is augmented with a system prompt and tool declarations
2. Gemini may request tool calls (`list_directory`, `read_file`, `write_file`)
3. The agent executes tools locally and sends results back
4. This loop repeats (up to 10 rounds) until Gemini responds with text


## WORK in Progress: Doesn't yet work:


#### Automatic Error Interception

The `ai-diagnose-error` function formats any Common Lisp condition object into a string and sends it to the agent. You can call it manually from the debugger:

```lisp
;; From within SBCL's debugger, after an error:
(ai-diagnose-error *)
```

Or, for a fully automatic workflow, you can hook it into SBCL's debugger hook. Add the following **optional** line if you want every unhandled error to be automatically diagnosed:

```lisp
;; Optional: auto-diagnose all unhandled errors
(setf *debugger-hook*
      (lambda (condition hook)
        (declare (ignore hook))
        (ai-diagnose-error condition)
        ;; Drop into the normal debugger afterward
        (invoke-debugger condition)))
```

With this hook active, any unhandled error will first print the AI's diagnosis and then drop into the normal SBCL debugger. Remove this line if the automatic diagnosis becomes too chatty during normal development.



* (/ 1 0)
--- AI Diagnosis ---
This error occurs because Common Lisp does not allow
division by zero. The expression (/ 1 0) attempts to
divide the integer 1 by 0, which is mathematically
undefined.

Fix: Add a guard before dividing:

  (let ((divisor 0))
    (if (zerop divisor)
        (error "Cannot divide by zero")
        (/ 1 divisor)))
--- End Diagnosis ---

debugger invoked on DIVISION-BY-ZERO ...
