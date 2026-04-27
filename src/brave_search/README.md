# Brave Web Search Client Library

**Book Chapter:** [Information Gathering](https://leanpub.com/read/lovinglisp/information-gathering) — *Loving Common Lisp* (free to read online).

A Common Lisp client for the Brave Search API. It performs web searches and returns structured results containing the URL, title, and description for each hit.

## Prerequisites

- **SBCL** with [Quicklisp](https://www.quicklisp.org/)
- A Brave Search API key — set the `BRAVE_SEARCH_API_KEY` environment variable. You can obtain a free key at [brave.com/search/api](https://brave.com/search/api/).

## Dependencies

- `uiop`, `cl-json`, `drakma`, `myutils`

## Usage

```lisp
(ql:quickload "brave_search")

;; Perform a web search
(brave_search:websearch "Sedona Arizona")
;; => (("https://..." "Sedona, AZ" "Sedona is a city in ...") ...)
```

## Available Functions

- `(brave_search:websearch query)` — Search the web and return a list of `(url title description)` results.
