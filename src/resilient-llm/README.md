# resilient-llm

Common Lisp condition system tutorial and LLM call recovery policies.

Low-level functions signal conditions and offer restarts. Callers install handlers to choose a recovery policy: retry with exponential backoff, fall back to another model, supply a replacement value, or ask the model to repair malformed JSON.

## Files

- `resilient-llm.asd`: ASDF system definitions for `:resilient-llm` and `:resilient-llm/tests`.
- `package.lisp`: Package `#:resilient-llm` (with local nickname `jzon` for `com.inuoe.jzon`) and exported symbols.
- `basics.lisp`: Small age-parsing example that demonstrates conditions, restarts, and handler policies.
- `resilient-llm.lisp`: `complete`, `complete-json`, retry with backoff, model fallback, and JSON repair.
- `tests.lisp`: Offline test suite that uses fake models and records delays without sleeping.
- `examples.lisp`: Live examples that call a local Ollama server.
- `Makefile`: Targets to run tests, live examples, and a REPL.

## Requirements

- SBCL (or another Common Lisp implementation) with Quicklisp.
- Quicklisp / ASDF systems: `litelm`, `usocket`, `com.inuoe.jzon`.
- For `examples.lisp` only: a running local Ollama server with `llama3.2:3b` (`ollama pull llama3.2:3b`). Offline tests need no LLM server.

## Running the Code

### With `make`

Run the offline test suite (default target):

```sh
make test
```

Run the live examples (requires local Ollama with `llama3.2:3b`):

```sh
make examples
```

Start an interactive SBCL REPL with `:resilient-llm` loaded:

```sh
make repl
```

Remove compiled `.fasl` files:

```sh
make clean
```

### From a REPL

Load the ASDF file, then run the offline tests:

```lisp
(asdf:load-asd (truename "resilient-llm.asd"))
(ql:quickload :resilient-llm/tests)
(resilient-llm::run-tests)
```

Run the live examples:

```lisp
(asdf:load-asd (truename "resilient-llm.asd"))
(ql:quickload :resilient-llm)
(load "examples.lisp")
```

If this directory is in Quicklisp's `local-projects`, you can omit the `asdf:load-asd` line.

## How It Works

### Condition System Basics (`basics.lisp`)

`parse-age` parses an integer from `0` to `150`. On invalid input, it signals `bad-age` with two restarts:

- `use-value`: return a replacement age.
- `skip-entry`: return `nil`, which `parse-ages` drops.

Three callers apply different policies to the same `parse-ages` function:

- `parse-ages-skipping`: invokes `skip-entry`.
- `parse-ages-with-default`: invokes `use-value` with a default integer.
- `parse-ages-logging`: logs each `bad-age` condition in an inner `handler-bind`, returns normally to decline it, and lets an outer `handler-bind` invoke `skip-entry`.

### Resilient LLM Calls (`resilient-llm.lisp`)

`complete` calls `*completion-function*` (default `litelm-completion`) with `:model` (default `*default-model*`, `"ollama/llama3.2:3b"`). On error, it offers three restarts:

- `retry`: call the same model again.
- `use-model`: switch to another model and try again.
- `use-value`: return a caller-supplied string.

Policy macros wrap `complete`:

- `with-retries` (`call-with-retries`): catches transient errors (`transient-error-p` matches `litelm:rate-limit-error`, `litelm:api-error` with HTTP status `>= 500`, and `usocket:socket-error`) and invokes `retry` up to `:max-retries` times (default `3`). Before sleeping via `*sleep-function*`, it computes jittered exponential backoff with `backoff-delay` (default `:base-delay 1.0`, `:max-delay 30.0`) and signals the non-error condition `retrying`.
- `with-fallback-models` (`call-with-fallback-models`): catches `litelm:litelm-error` and transient errors, then invokes `use-model` with each fallback model in order.

You can nest policies. Place `with-fallback-models` outside `with-retries` so transient errors retry first and then fall back, while non-transient errors (such as HTTP 404) skip retries and fall back right away:

```lisp
(handler-bind ((resilient-llm:retrying
                 (lambda (c) (format t "~&~A~%" c))))
  (resilient-llm:with-fallback-models ("ollama/llama3.2:3b")
    (resilient-llm:with-retries (:max-retries 3 :base-delay 0.5)
      (resilient-llm:complete "Name one Common Lisp implementation."
                              :model "ollama-down/llama3.2:3b"))))
```

### JSON Replies and Repair (`resilient-llm.lisp`)

- `strip-code-fences`: removes surrounding Markdown fences such as `` ```json ... ``` ``.
- `parse-json-reply`: strips fences and parses with `jzon:parse`, signaling `malformed-json` on parse errors.
- `complete-json`: calls `complete`, parses the reply, and offers `repair-json` and `use-value` restarts on `malformed-json`.
- `with-json-repair`: handles `malformed-json` by invoking `repair-json` up to `:max-repairs` times (default `2`), sending the parse error and invalid reply back to the model.

```lisp
(resilient-llm:with-json-repair (:max-repairs 2)
  (resilient-llm:complete-json
   "Return a JSON object with keys \"name\" and \"year\" for Common Lisp."))
```

### Offline Testing Hooks

Bind `*completion-function*` to a `(lambda (model prompt) ...)` fake model and `*sleep-function*` to a no-op to test retry, fallback, and repair policies without network access or delays. See `tests.lisp` for examples.
