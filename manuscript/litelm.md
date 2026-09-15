# One Library, Many LLM Providers: the litelm Library

**Dear reader, earlier editions of this book had separate chapters for each LLM provider, each with its own little client library. That was a lot of repeated code! In this edition we implement one small Common Lisp library, litelm, that talks to many providers through a single code path, and we will reuse it in several later chapters.**

The idea behind **litelm** is simple: most hosted LLM services now speak the same "OpenAI compatible" HTTP API, so if we build one chat-completion payload and one embedding payload, the only thing that changes from provider to provider is the base URL and where the API key comes from. The library is modeled after the Python [litelm](https://github.com/kennethwolters/litelm) library (itself a stripped-down litellm library), but here the code has an interesting: messages and tool definitions are written in a Common Lisp friendly nested list format, so you never see JSON in your own application code. Not dealing with JSON in Common Lisp application code is a good quality of life improvement.

The source code lives in the directory **loving-common-lisp/src/litelm**, and the only dependencies are **dexador** for HTTP and **uiop** (the JSON encoder/decoder is a self-contained ~150 line implementation in the library itself, which we will peek at later).

Here are the five providers that are built in:

| Key | Base URL | Key env vars |
|---|---|---|
| `openai` | `https://api.openai.com/v1` | `OPENAI_API_KEY`, `OPENAI_KEY` |
| `gemini` | `https://generativelanguage.googleapis.com/v1beta/openai` | `GEMINI_API_KEY`, `GOOGLE_API_KEY` |
| `fireworks-ai` | `https://api.fireworks.ai/inference/v1` | `FIREWORKS_API_KEY` |
| `deepseek` | `https://api.deepseek.com/v1` | `DEEPSEEK_API_KEY` |
| `ollama` | `http://localhost:11434/v1` | none |

Gemini might surprise you in this table: Google serves an OpenAI compatible chat endpoint at that base URL, so it fits right in. Ollama running local models needs no key at all since it runs on your own machine, which makes it perfect for trying out the examples in this chapter for free.

## The Provider Registry

Everything starts in **providers.lisp** with a tiny struct that captures all we need to know about a provider:

```lisp
(defstruct provider
  name          ; keyword, e.g. :openai
  base-url      ; OpenAI-compatible base URL, e.g. "https://api.openai.com/v1"
  env-keys      ; list of environment variables to try for the API key
  (requires-key t))
```

We keep the known providers in a hash table keyed by keyword:

```lisp
(defvar *providers* (make-hash-table :test 'eq))
```

The function **define-provider** registers (or re-registers) an entry, and the five built in providers are defined at load time with plain calls to it. This means you can add your own provider at run time, at the REPL, in one line:

```lisp
(litelm:define-provider :fireworks-ai "https://api.fireworks.ai/inference/v1"
  :env-keys '("FIREWORKS_API_KEY"))
```

Lookups go through **find-provider**, which I deliberately made `fail fast`: if you typo a provider name, it signals a **litelm-error** that helpfully lists the known provider names, before any network traffic happens. Small affordances like this save debugging time.

## Model Strings: "provider/model-name"

You address a model with a single string, which keeps call sites tidy:

```lisp
(litelm:completion "deepseek/deepseek-flash"
  :messages '((:system "Be terse.")
              (:user "What is 2+2?")))
```

The function **parse-model** splits the string on the *first* slash only. That detail matters, because some providers (Fireworks AI in particular) use model names that contain slashes of their own:

```lisp
"fireworks-ai/accounts/fireworks/models/deepseek-v4-flash"
```

Here the provider is `fireworks-ai` and the model name is the rather long remainder, `accounts/fireworks/models/deepseek-v4-flash`. If you ever want to skip the prefix parsing, **parse-model** also accepts a `:provider` keyword argument, and a string with no slash at all signals a **litelm-error** with a friendly message.

## Finding the API Key

The function **provider-api-key** resolves a key in a fixed, easy to remember order:

1. Use the `:api-key` argument from the call, if you passed one.
2. Otherwise scan the provider's `env-keys` in order and use the first environment variable that is set and non blank.
3. If still nothing, signal a **litelm-error** when the provider requires a key.
4. Or, for key-free providers, quietly return nil.

Ollama is registered with `:requires-key nil` and an empty `env-keys` list, so local calls just work. I like this ordering because it lets test code pass a short-lived key explicitly while application code simply relies on environment variables.

## Headers and URLs: Small Pure Helpers

Two little pure functions finish the plumbing. **provider-headers** builds a fixed `Content-Type: application/json` pair, plus an `Authorization: Bearer KEY` pair when a key exists. **provider-url** joins base and path:

```lisp
(defun provider-url (provider path api-base)
  (concatenate 'string (or api-base (provider-base-url provider)) path))
```

The **completion** entry point calls it with `"/chat/completions"` and **embedding** calls it with `"/embeddings"`. Because a per-call `:api-base` can override the host, and `:extra-headers` can add header pairs, you can point a known provider at a proxy, a mock server, or a test host without touching the registry at all. I use this trick constantly when writing tests.

## The Shared Send Path in completion

The abstractions we have been talking about have this benefit: **completion** runs exactly the same steps for every provider. There is not a single `cond` on provider name in the whole function.

1. Call **parse-model** to get the provider struct and the bare model name.
2. Call **provider-api-key** and **provider-headers** to build the auth headers.
3. Call **provider-url** to form the full POST URL.
4. Send the bare model name in `"model"` (not the full `"provider/model"` string).
5. Translate the Lisp format messages and tool definitions into wire format alists.
6. Add `:temperature`, `:max-tokens`, `:top-p`, `:tool-choice`, and `:stream` only when the caller actually set them.
7. POST and parse the OpenAI style reply.

The payload construction shows steps 4 through 6 as plain `append` and `when` expressions, no specialized code required:

```lisp
(payload
  (append
   `(("model" . ,model-name)
     ("messages" . ,(translate-messages messages)))
   (when tools
     `(("tools" . ,(translate-tools tools))))
   (when tool-choice
     `(("tool_choice" . ,(%name-string tool-choice))))
   (when temperature `(("temperature" . ,temperature)))
   (when max-tokens `(("max_tokens" . ,max-tokens)))
   (when top-p `(("top_p" . ,top-p)))
   (when stream `(("stream" . t)))))
```

Since every provider speaks the same chat shape, a new provider that speaks this shape works immediately with one **define-provider** call, with no other code changes. That is the whole payoff of the design.

## Embeddings Share the Same Path

The **embedding** function reuses steps 1 through 4 and then posts to `"/embeddings"` instead:

```lisp
(litelm:embedding "openai/text-embedding-3-small" '("hello" "world"))
```

It wraps a lone string in a list for you, sends `"model"` plus `"input"`, and collects each `"embedding"` vector from the `"data"` list in the reply. Because key and base URL resolution are shared, a local Ollama embedding model and a cloud embedding model differ only in the prefix of the model string.

## Messages and Tools in Lisp Clothing

The file **messages.lisp** is where the Common Lisp friendly format gets translated to the wire format, once, for all providers. Here is the format you write, taken from the file's own header comment:

```lisp
;;; Message format:
;;;   ((:system "You are terse.")
;;;    (:user "What's the weather in Paris?")
;;;    (:assistant nil :tool-calls ((:id "call_1" :name get_weather
;;;                                   :arguments "{\"location\":\"Paris\"}")))
;;;    (:tool "sunny, 22C" :tool-call-id "call_1"))
;;;
;;; Tool definition format:
;;;   ((get_weather "Get the current weather for a location"
;;;      ((location "string" "City name")              ; required by default
;;;       (units "string" "celsius or fahrenheit" :required nil
;;;              :enum ("celsius" "fahrenheit")))))
```

Symbols like `get_weather` are downcased to strings, a plain string as the whole `messages` argument becomes a single user turn, and tool parameters default to being required unless you say `:required nil`. On the way back, the reply parser turns `tool_calls` into plists with keyword keys, and turns the `usage` object into a plist of `(:prompt-tokens n :completion-tokens n :total-tokens n)`.

Since every provider reports those same three usage fields, your token accounting code never has to branch on provider either: prompt tokens plus completion tokens equals total tokens, everywhere.

## Streaming and Errors, Same Everywhere

Streaming mode posts the same payload plus `"stream": true`, reads Server-Sent Events `data:` lines, calls your chunk function with each text delta (or just prints them if you pass `:stream t`), and still accumulates the chunks into `response-content` so you get the full text at the end. Non-streaming mode reads `choices`, `message`, `tool_calls`, `finish_reason`, `model`, and `usage` out of the same JSON shape regardless of which provider answered.

HTTP errors are mapped the same way for every host, into a little condition hierarchy that mirrors the Python `litelm` library:

```
litelm-error
└── api-error              (readers: api-error-status, api-error-body)
    ├── authentication-error        401, 403
    ├── rate-limit-error            429
    ├── not-found-error             404
    └── context-window-exceeded-error  400 mentioning "context"
```

This means you write *one* **handler-case** and it works for every provider:

```lisp
(handler-case (litelm:completion "openai/gpt-4o" :messages "...")
  (litelm:rate-limit-error () ...)          ; back off and retry
  (litelm:authentication-error () ...)      ; bad or missing API key
  (litelm:context-window-exceeded-error () ...))
```

## Trying It Out

The easiest way to play with this code is with a local Ollama server, since it needs no API key. Here is a short REPL session (I am using the small `qwen3-vl:2b` model locally):

```lisp
$ sbcl
* (asdf:load-asd "litelm.asd")
* (asdf:load-system :litelm)
...
* (litelm:response-content
    (litelm:completion "ollama/qwen3-vl:2b"
                       :messages '((:system "Answer in one word.")
                                   (:user "What is 2+2?"))
                       :max-tokens 4096))
"4"
```

Streaming is just as friendly, just pass a function of one argument and it is called with each text delta as it arrives:

```lisp
* (litelm:completion "ollama/qwen3-vl:2b"
                     :messages "Count from 1 to 5."
                     :stream (lambda (delta) (write-string delta)))
1, 2, 3, 4, 5
#S(LITELM:RESPONSE :CONTENT "1, 2, 3, 4, 5" ...)
```

And here is tool calling, where the model asks *us* to run a function and we simply read the request off the response struct so tool calls are returned, never executed, so your Lisp code stays in charge:

```lisp
* (let ((tools '((get_weather "Get the current weather for a location"
                  ((location "string" "City name"))))))
    (litelm:response-tool-calls
      (litelm:completion "ollama/qwen3-vl:2b"
                         :messages "What is the weather in Paris?"
                         :tools tools
                         :max-tokens 4096)))
((:ID "call_1" :NAME "get_weather" :ARGUMENTS ((:LOCATION . "Paris"))))
```

The file **tests.lisp** in the source directory contains a full two-turn tool calling example, where the tool result is sent back to the model for a final natural language answer, plus offline tests for the JSON encoder, message translation, and model routing. You can run it like this:

```bash
sbcl --no-userinit --non-interactive \
  --eval '(load "~/quicklisp/setup.lisp")' \
  --eval '(asdf:load-asd "litelm.asd")' \
  --eval '(asdf:load-system :litelm)' \
  --load tests.lisp
```

If an Ollama server is reachable, the script also runs live completion, streaming, and tool calling tests; otherwise it quietly sticks to the offline tests. The output ends with a line like `0 failure(s).`

## Adding a Provider or Swapping an Endpoint

When you want to reach a new host, you have three options, from lightest to most permanent:

1. **Per-call override:** pass `:api-base` and `:api-key` to a single **completion** call. Perfect for a proxy or a quick experiment.
2. **Runtime registration:** call **define-provider** with a base URL and env vars. Ideal for a lasting OpenAI compatible host such as DeepSeek or Fireworks AI.
3. **Key-free local host:** use (or copy) the Ollama entry with `:requires-key nil` for anything on your own machine or LAN.

Because routing, authentication, sending, and parsing are kept separate, option 2 is literally one line of code and everything else in the library keeps working unchanged.

## litelm Chapter Wrap Up

Dear reader, this chapter was short on purpose: the whole point of **litelm** is that talking to five different LLM providers should not take five different libraries. We saw how a tiny provider registry, a `"provider/model-name"` naming convention, and one shared send path let us chat, stream, call tools, and compute embeddings through a single API. Just as importantly, we write messages and tool definitions as ordinary nested Lisp lists (no JSON) and get back a tidy **response** struct and a friendly condition hierarchy when things go wrong.

In later chapters we will build on this foundation, swapping providers by changing a single string. I hope you come to appreciate, as I do, how a small amount of thoughtful Common Lisp code can smooth over a lot of web API tedium.

## Optional Practice Problems

1. **Register a New Provider:** Use `litelm:define-provider` to register a provider for an OpenAI compatible service you have access to (for example Groq at `https://api.groq.com/openai/v1`). Verify with `litelm:find-provider` that the struct was stored correctly, then make a real completion call using only environment variables for the key.

2. **Mock Server Testing:** Start a tiny local HTTP server (even `python3 -m http.server` will not be enough here since you need something that answers POSTs; a few lines of Lisp with Hunchentoot works nicely) that returns a canned OpenAI style chat response. Use the `:api-base` argument to point `litelm:completion` at your mock and verify that `response-content` parses correctly.

3. **Usage Tracker:** Write a function `completion-with-cost` that wraps `litelm:completion`, reads the `response-usage` plist, and keeps a running total of prompt and completion tokens in a global hash table keyed by provider keyword. Print a little spending report.

4. **Retry on Rate Limit:** Using the condition hierarchy from this chapter, write a wrapper that catches `litelm:rate-limit-error`, waits a few seconds (see `sleep`), and retries the call up to three times before giving up. Bonus: use an exponential backoff.

5. **Two-Turn Tool Loop:** Following the example in `tests.lisp`, write a complete two-turn tool calling conversation: register a `calculate` tool, return its result as a `(:tool ...)` message, and print the model's final natural language answer. Try it with both a local Ollama model and a cloud provider and compare how reliably each calls the tool.
