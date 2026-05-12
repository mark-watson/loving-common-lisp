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

### Debug mode

```lisp
(setf cl-ai-coding-agent:*verbose* t)
(cl-ai-coding-agent:coding-agent-query "...")
;; Prints tool calls, responses, and round info
```

## Exported API

| Symbol | Type | Description |
|--------|------|-------------|
| `coding-agent-query` | Function | `(prompt) → string` — one-shot agent query |
| `coding-agent-repl` | Function | `() → nil` — interactive REPL loop |
| `*verbose*` | Variable | When non-NIL, prints debug info |

## Architecture

The agent uses Gemini's Interactions API with function calling:

1. User prompt is augmented with a system prompt and tool declarations
2. Gemini may request tool calls (`list_directory`, `read_file`, `write_file`)
3. The agent executes tools locally and sends results back
4. This loop repeats (up to 10 rounds) until Gemini responds with text
