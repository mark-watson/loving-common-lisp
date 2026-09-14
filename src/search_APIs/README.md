# search-apis — one Common Lisp interface to web search APIs

A single, uniform web search codebase for **Brave**, **Tavily**, and
**Perplexity**. It is modeled after the sibling `../litelm` library: a small
provider registry, a shared result format, a common error hierarchy, and a
self-contained JSON codec, so the only dependencies are `dexador`, `quri`, and
`uiop`.

Each provider has a very different wire protocol — Brave is a GET with the key in
a header, Tavily is a POST with the key in the body, and Perplexity is a
search-plus-LLM chat completion — but they all return the same
`search-response` shape.

## Installation

```lisp
(asdf:load-asd "/path/to/search_APIs/search-apis.asd")
(asdf:load-system :search-apis)
```

Dependencies (via Quicklisp): `dexador`, `quri`.

## Providers

| Provider | Keyword | API key env var | Endpoint |
|---|---|---|---|
| Brave | `:brave` | `BRAVE_SEARCH_API_KEY` | `https://api.search.brave.com/res/v1/web/search` |
| Tavily | `:tavily` | `TAVILY_API_KEY` | `https://api.tavily.com/search` |
| Perplexity | `:perplexity` | `PERPLEXITY_API_KEY` | `https://api.perplexity.ai/chat/completions` |

## Usage

```lisp
;; Brave and Tavily return a list of hits.
(let ((resp (search-apis:websearch "Sedona Arizona" :provider :brave :max-results 5)))
  (dolist (r (search-apis:search-response-results resp))
    (format t "~A~%  ~A~%  ~A~%"
            (search-apis:search-result-title r)
            (search-apis:search-result-url r)
            (search-apis:search-result-content r))))

;; Perplexity also synthesizes an answer and returns citations.
(let ((resp (search-apis:websearch "Where is Sedona Arizona?" :provider :perplexity)))
  (format t "~A~%" (search-apis:search-response-answer resp))
  (dolist (r (search-apis:search-response-results resp))
    (format t "  ~A~%" (search-apis:search-result-url r))))

;; Pass a key explicitly instead of via the environment.
(search-apis:websearch "Sedona" :provider :tavily :api-key "...")
```

`websearch` accepts `:provider`, `:api-key`, `:max-results` (used by Brave and
Tavily), and `:model` (used by Perplexity, default `"sonar-pro"`). Provider
functions ignore options that do not apply to them.

## Shared result format

```lisp
(defstruct search-result  title url content score)
(defstruct search-response provider query answer results raw)
```

- `search-response-results` — list of `search-result`; each has `title`, `url`,
  `content` (the provider's snippet/summary), and a provider-specific `score`.
- `search-response-answer` — the synthesized answer, for search-plus-LLM
  providers such as Perplexity; `nil` for pure search providers.
- `search-response-raw` — the full decoded JSON, for anything the structs omit.

## Registering a provider

Any provider whose search function returns a `search-response` can be added at
runtime:

```lisp
(search-apis:define-search-provider :my-engine "https://api.example.com/search"
  :env-keys '("MY_ENGINE_API_KEY")
  :function #'my-engine-search)
```

The function is called as `(fn provider query :api-key key :max-results n :model m
&allow-other-keys)` and must return a `search-response`.

## Error handling

HTTP failures map onto a condition hierarchy mirroring `litelm`:

```
search-error
└── api-error              (readers: api-error-status, api-error-body)
    ├── authentication-error   401, 403
    ├── rate-limit-error       429
    └── not-found-error        404
```

```lisp
(handler-case (search-apis:websearch "..." :provider :tavily)
  (search-apis:rate-limit-error () ...)      ; back off
  (search-apis:authentication-error () ...)) ; bad API key
```

## Tests

```bash
sbcl --no-userinit --non-interactive \
  --eval '(load "~/quicklisp/setup.lisp")' \
  --eval '(asdf:load-asd "search-apis.asd")' \
  --eval '(asdf:load-system :search-apis)' \
  --load tests.lisp
```

Offline tests cover JSON, the provider registry, and the response parsers. Live
tests run only for providers whose API key environment variable is set.

## File structure

| File | Contents |
|---|---|
| `search-apis.asd` | ASDF system definition |
| `package.lisp` | Package definition and exports |
| `json.lisp` | Self-contained JSON encoder/decoder |
| `search-apis.lisp` | Result structs, conditions, registry, `websearch` |
| `providers.lisp` | Brave, Tavily, and Perplexity request/response handling |
| `tests.lisp` | Offline parser tests + optional live tests |

Copyright (C) 2026 Mark Watson — MIT License
