# LLM Test

Example scripts for testing the `llm` Common Lisp library with multiple LLM backends. Each file demonstrates basic completions and tool calling using a different provider.

## Files

### `simple-tools-test.lisp`

Standalone demo of the `simple-tools` system independent of any LLM backend. Defines and calls three example tools directly:

- `add-numbers` — adds two numbers and returns a formatted string
- `get-current-time` — returns the current decoded time
- `capitalize-text` — uppercases a string

Also prints all registered tools from the `simple-tools:*tools*` hash table.

### `openai_test.lisp`

Tests the `openai` backend with:

- A plain completion (`openai:completions`) asking "Why is the sky blue?"
- Tool calling: asks for the weather in Paris and dispatches to a `get-weather` tool that returns a hardcoded temperature

### `ollama_test.lisp`

Same structure as `openai_test.lisp` but uses the `ollama` backend (`ollama:completions`). Useful for running tests locally without an API key.

### `gemini_test.lisp`

Tests the `gemini` backend with three scenarios:

- Plain generation (`gemini:generate`)
- Generation with Google Search grounding (`gemini:generate-with-search`)
- Tool calling with the same `get-weather` tool

## Usage

Load any file with a running Lisp image that has Quicklisp and the `llm` system available:

```lisp
(load "openai_test.lisp")
```

or from the REPL:

```lisp
(ql:quickload :llm)
(load "gemini_test.lisp")
```

### Prerequisites

- [Quicklisp](https://www.quicklisp.org/) with the `llm` system installed
- API keys set in the environment as expected by each backend (e.g. `OPENAI_API_KEY`, `GEMINI_API_KEY`)
- For Ollama: a running local Ollama instance

## Tool Definition

All provider test files use `simple-tools:define-tool` to register tools:

```lisp
(simple-tools:define-tool get-weather
    ((location string "The city and state, e.g. San Francisco, CA")
     (unit string "The unit of temperature, e.g. 'c' or 'f'"))
    "Get the current weather in a given location"
  (if (equal unit "c") "22" "72"))
```

The tool registry (`simple-tools:*tools*`) is shared across packages, so tools defined in one test file are visible to any backend that looks them up by name.
