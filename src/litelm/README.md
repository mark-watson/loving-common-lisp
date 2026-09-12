# litelm — Common Lisp LLM routing library

Minimal LLM call routing and message translation across providers, modeled after
the Python [litelm](https://github.com/kennethwolters/litelm) library
(itself a stripped-down litellm): just the call path — model routing, message
translation, streaming, tool use, embeddings — and nothing else. No router class,
no proxy, no caching.

Unlike the Python library (and the sibling `../llm` library), messages and tool
definitions are written in a **Common Lisp friendly nested list format** with
symbols, strings, and numbers — no JSON anywhere in user code. JSON encoding and
decoding is handled internally by a self-contained ~150-line implementation, so
the only dependencies are `dexador` and `uiop`.

## Installation

```lisp
(asdf:load-asd "/path/to/litelm/litelm.asd")
(asdf:load-system :litelm)
```

Dependencies (via Quicklisp): `dexador`.

## Model routing

Models are addressed as `"provider/model-name"` strings:

| Provider | Prefix | API key env vars | Base URL |
|---|---|---|---|
| OpenAI | `openai/` | `OPENAI_API_KEY`, `OPENAI_KEY` | `https://api.openai.com/v1` |
| Gemini | `gemini/` | `GEMINI_API_KEY`, `GOOGLE_API_KEY` | `https://generativelanguage.googleapis.com/v1beta/openai` |
| Fireworks AI | `fireworks-ai/` | `FIREWORKS_API_KEY` | `https://api.fireworks.ai/inference/v1` |
| DeepSeek | `deepseek/` | `DEEPSEEK_API_KEY` | `https://api.deepseek.com/v1` |
| Ollama (local) | `ollama/` | — | `http://localhost:11434/v1` |

All five are OpenAI-compatible chat endpoints; the library shares one code path
and routes via the prefix. Model names may themselves contain slashes (e.g.
`"fireworks-ai/accounts/fireworks/models/deepseek-v4-flash-0731"`).

Register additional OpenAI-compatible providers at runtime:

```lisp
(litelm:define-provider :groq "https://api.groq.com/openai/v1"
  :env-keys '("GROQ_API_KEY"))
```

Any provider's endpoint can be overridden per call with `:api-base`, and the key
with `:api-key`.

## Message format

A message is a list `(role content &rest options)` where `role` is a symbol
(`:system` `:user` `:assistant` `:tool`), `content` is a string (or `nil`), and
options is a plist. A plain string is shorthand for a single user message.

```lisp
'((:system "You are terse.")
  (:user "What is the weather in Paris?")
  (:assistant nil :tool-calls ((:id "call_1" :name get_weather
                                :arguments "{\"location\":\"Paris\"}")))
  (:tool "sunny and 22C" :tool-call-id "call_1"))
```

## Tool definition format

```lisp
'((get_weather "Get the current weather for a location"
    ((location "string" "City name")                 ; required by default
     (units "string" "celsius or fahrenheit" :required nil
            :enum ("celsius" "fahrenheit"))))
  (calculator "Evaluate an arithmetic expression"
    ((expression "string" "Expression to evaluate"))))
```

Each tool is `(name description ((param-name param-type param-description
&key (required t) enum) ...))`. Names may be symbols or strings.

## Usage

```lisp
;; basic completion — returns a LITELM:RESPONSE struct
(let ((r (litelm:completion "deepseek/deepseek-chat"
                            :messages '((:system "Be terse.")
                                        (:user "What is 2+2?")))))
  (litelm:response-content r))           ; => "4"

;; string shorthand
(litelm:response-content
  (litelm:completion "gemini/gemini-2.5-flash" :messages "Hello!"))

;; streaming: pass a function (called with each delta) or t (print deltas)
(litelm:completion "ollama/qwen3-vl:2b"
                   :messages "Count to five."
                   :stream (lambda (delta) (write-string delta)))

;; tool calling: tool calls are *returned*, not executed
(let* ((tools '((get_weather "Get the current weather for a location"
                 ((location "string" "City name")))))
       (r (litelm:completion "openai/gpt-4o"
                             :messages "Weather in Paris?"
                             :tools tools
                             :tool-choice :auto)))
  (litelm:response-tool-calls r))
;; => ((:id "call_abc" :name "get_weather" :arguments ((:location . "Paris"))))

;; embeddings
(litelm:embedding "openai/text-embedding-3-small" '("hello" "world"))
```

Other keyword arguments to `completion`: `:temperature`, `:max-tokens`,
`:top-p`, `:tool-choice` (`:auto` / `:none` / `:required`), `:api-key`,
`:api-base`, `:extra-headers`.

The response struct accessors: `response-content`, `response-tool-calls`,
`response-finish-reason`, `response-model`, `response-usage` (a plist
`(:prompt-tokens n :completion-tokens n :total-tokens n)`), and
`response-raw` (the full decoded JSON alist).

## Error handling

Provider HTTP errors map onto a condition hierarchy mirroring litelm:

```
litelm-error
└── api-error              (readers: api-error-status, api-error-body)
    ├── authentication-error        401, 403
    ├── rate-limit-error            429
    ├── not-found-error             404
    └── context-window-exceeded-error  400 mentioning "context"
```

```lisp
(handler-case (litelm:completion "openai/gpt-4o" :messages "...")
  (litelm:rate-limit-error () ...)          ; back off
  (litelm:authentication-error () ...)      ; bad API key
  (litelm:context-window-exceeded-error () ...))
```

## Tests

```bash
sbcl --no-userinit --non-interactive \
  --eval '(load "~/quicklisp/setup.lisp")' \
  --eval '(asdf:load-asd "litelm.asd")' \
  --eval '(asdf:load-system :litelm)' \
  --load tests.lisp
```

Offline tests cover JSON encode/decode, message and tool translation, and model
routing. If an Ollama server is reachable at `localhost:11434`, live tests also
exercise completion, streaming, and the full two-turn tool-calling protocol.

## File structure

| File | Contents |
|---|---|
| `litelm.asd` | ASDF system definition |
| `package.lisp` | Package definition and exports |
| `json.lisp` | Self-contained JSON encoder/decoder |
| `providers.lisp` | Provider registry and `"provider/model"` routing |
| `messages.lisp` | Lisp format ⇄ wire format translation |
| `litelm.lisp` | `completion`, `embedding`, streaming, condition hierarchy |
| `tests.lisp` | Offline + live (Ollama) tests |

Copyright (C) 2026 Mark Watson — MIT License
