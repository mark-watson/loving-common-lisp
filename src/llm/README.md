# Common Lisp LLM Library

A Common Lisp library providing a unified interface for multiple LLM providers (Google Gemini, OpenAI, and Ollama) with tool/function-calling support.

Copyright (C) 2026 Mark Watson — Apache 2 / MIT License

## Features

- **Multi-provider support**: Google Gemini, OpenAI, and local Ollama models
- **Tool/function calling**: Define Lisp functions as LLM-callable tools with a simple macro
- **Lightweight**: Uses `curl` for HTTP and `cl-json` for JSON — no heavy HTTP client dependencies
- **Unified tool registry**: Tools defined once work across all providers

## Dependencies

- [ASDF](https://asdf.common-lisp.dev/) — build system
- [Quicklisp](https://www.quicklisp.org/) — package manager
- [`cl-json`](https://common-lisp.net/project/cl-json/) — JSON encoding/decoding
- `curl` — must be available on `PATH`

## Installation

Load via ASDF:

```lisp
(asdf:load-asd "/path/to/llm/llm.asd")
(asdf:load-system :llm)
```

Or use `project.lisp` to load everything at once:

```lisp
(load "project.lisp")
```

## API Keys / Environment Variables

Set the following environment variables before use:

| Variable | Provider |
|---|---|
| `OPENAI_KEY` | OpenAI |
| `GOOGLE_API_KEY` | Google Gemini |

Ollama requires no API key — it runs locally at `http://localhost:11434`.

## Usage

### Gemini

```lisp
(in-package :gemini)

;; Generate text
(generate "What is the capital of France?")

;; Count tokens
(count-tokens "Hello, world!")

;; Interactive chat session (REPL loop)
(chat)

;; Generate with Google Search grounding
(generate-with-search "What happened in the news today?")

;; Generate with search and return citations
(multiple-value-bind (text citations)
    (generate-with-search-and-citations "Latest AI research breakthroughs")
  (format t "~A~%~%Citations:~%~{~A~%~}" text citations))
```

Default model: `gemini-3-flash-preview`. Override by passing a model ID as the last argument or setting `*gemini-model*`.

### OpenAI

```lisp
(in-package :openai)

;; Chat completion
(completions "Explain quantum entanglement in one sentence.")

;; Convenience wrapper
(answer-question "What is the speed of light?")
```

Default model: `gpt-4o-mini`. Override by passing a model ID or setting `*openai-model*`.

### Ollama (local models)

```lisp
(in-package :ollama)

;; Chat completion
(completions "Write a haiku about Lisp.")

;; Summarize text
(summarize "Long text to summarize...")

;; Answer a question
(answer-question "What is tail-call optimization?")
```

Default model: `mistral:v0.3`. Override by passing a model ID or setting `*ollama-model*`.

## Tool / Function Calling

The `simple-tools` package provides a `define-tool` macro that registers Lisp functions as LLM-callable tools.

### Defining a tool

```lisp
(in-package :simple-tools)

(define-tool get-weather
    (("location" "string" "The city and country to get weather for"))
  "Get the current weather for a location."
  (format nil "The weather in ~A is sunny and 22°C." location))
```

### Using tools with a provider

Pass a list of tool symbols to any provider's `completions` or `generate` function:

```lisp
;; With OpenAI
(openai:completions "What's the weather in Paris?" '(get-weather))

;; With Ollama
(ollama:completions "What's the weather in Tokyo?" '(get-weather))

;; With Gemini
(gemini:generate "What's the weather in London?" '(get-weather))
```

When the model decides to call a tool, the library dispatches the call automatically and returns the result.

### Tool registry

All tools are stored in `simple-tools:*tools*` (a hash table keyed by name string). You can inspect or call tools directly:

```lisp
;; Call a tool by name
(simple-tools:call-tool "get-weather" "Paris, France")
```

## File Structure

| File | Package | Description |
|---|---|---|
| `llm.asd` | — | ASDF system definition |
| `llm.lisp` | `llm` | Shared utilities: `run-curl-command`, `escape-json`, `substitute-subseq` |
| `simple-tools.lisp` | `simple-tools` | Tool registry and `define-tool` macro |
| `gemini.lisp` | `gemini` | Google Gemini API client |
| `openai.lisp` | `openai` | OpenAI API client |
| `ollama.lisp` | `ollama` | Ollama local model client |
| `project.lisp` | — | Convenience loader script |
