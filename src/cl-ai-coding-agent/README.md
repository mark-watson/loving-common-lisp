# cl-ai-coding-agent

A Common Lisp AI coding agent powered by litelm. The agent
takes a string prompt as input and can autonomously read directories,
read files, write new files, and diagnose stacktraces/error messages.

The default model is the local Ollama model `qwen3.5:4b`,
so it runs without an API key. Any other litelm
`provider/model` string can be passed per call.

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
- Ollama running locally with the model pulled:
  `ollama pull qwen3.5:4b`
- The `litelm` library (in `../litelm`)

## Installation

```bash
ollama pull qwen3.5:4b
```

In your Lisp REPL:

```lisp
(asdf:load-asd "/path/to/litelm/litelm.asd")
(asdf:load-asd "/path/to/cl-ai-coding-agent/cl-ai-coding-agent.asd")
(ql:quickload :cl-ai-coding-agent)
```

## Usage

All queries default to `ollama/qwen3.5:4b`.
Pass `:model` to use another litelm model string.

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

;; Use a different model
(cl-ai-coding-agent:coding-agent-query
  "What files are here?"
  :model "gemini/gemini-2.5-flash")
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
Lisp string. Save the error output to a file instead:

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
;; Prints model, tool calls, responses, and round info
```

## Exported API

| Symbol | Type | Description |
|--------|------|-------------|
| `coding-agent-query` | Function | `(prompt &key model) -> string` -- one-shot agent query, defaults to `ollama/qwen3.5:4b` |
| `coding-agent-query-file` | Function | `(path &optional prefix model) -> string` -- query from file |
| `coding-agent-repl` | Function | `() -> nil` -- interactive REPL loop |
| `*verbose*` | Variable | When non-NIL, prints debug info |
| `*default-model*` | Variable | litelm model string, default `"ollama/qwen3.5:4b"` |
| `*max-tool-rounds*` | Variable | Max tool round-trips, default 10 |

## Architecture

The agent uses litelm `completion` with tool calling:

1. User prompt is paired with a system prompt and litelm tool definitions
2. litelm may return tool calls (`list_directory`, `read_file`, `write_file`)
3. The agent executes tools locally and appends `:tool` results to the message history
4. This loop repeats (up to 10 rounds) until litelm responds with text


## Automatic error interception lives in another chapter

`ai-diagnose-error` -- the function that formats a Common Lisp condition object
into a diagnostic prompt and sends it to this agent -- is **not** part of this
system. It is defined in the **Hacking the SBCL REPL** chapter
(`manuscript/hacking_SBCL_repl.md`), along with the `#?` reader macro and the
`*debugger-hook*` wiring that make it fire automatically. Loading only
`cl-ai-coding-agent` will not give you an `ai-diagnose-error` function.

A session there looks like this:

```text
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
```
