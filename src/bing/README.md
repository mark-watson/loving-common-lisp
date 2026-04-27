# Bing Web Search Client Library

**Book Chapter:** [Information Gathering](https://leanpub.com/read/lovinglisp/information-gathering) — *Loving Common Lisp* (free to read online).

A lightweight Common Lisp client for the Microsoft Bing Web Search v7 API. It returns structured search results (name, display URL, snippet) and includes a convenience function for looking up Wikidata URIs.

## Prerequisites

- **SBCL** with [Quicklisp](https://www.quicklisp.org/)
- A Bing Search v7 subscription key and endpoint — set the environment variables:
  - `BING_SEARCH_V7_SUBSCRIPTION_KEY`
  - `BING_SEARCH_V7_ENDPOINT`

## Dependencies

- `uiop`, `cl-json`, `drakma`, `myutils`

## Usage

```lisp
(ql:quickload "bing")

;; General web search — returns a list of (name url snippet) results
(bing:websearch "Berlin")

;; Look up a Wikidata URI for a topic
(bing:get-wikidata-uri "Sedona Arizona")
```

## Available Functions

- `(bing:websearch query)` — Perform a web search and return structured results.
- `(bing:get-wikidata-uri query)` — Search for a Wikidata entity URI via Bing.
