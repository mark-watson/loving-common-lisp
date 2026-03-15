# lightpanda — Common Lisp interface to the Lightpanda headless browser: NOT YET IN BOOK

Shells out to the [Lightpanda](https://github.com/lightpanda-io/browser) binary
to fetch JS-rendered web pages. No server process required.

## Dependencies

- [`lightpanda`](https://github.com/lightpanda-io/browser/releases/tag/nightly) binary on your PATH
- ASDF / Quicklisp

## Load

```lisp
(ql:quickload :lightpanda)
(in-package #:lightpanda)
```

## Usage

```lisp
;; Fetch JS-rendered HTML
(fetch-url "https://markwatson.com/")

;; Fetch as Markdown (good for LLM input)
(fetch-url "https://markwatson.com/" :dump "markdown")

;; Other :dump options: "semantic_tree"  "semantic_tree_text"

;; Respect robots.txt
(fetch-url "https://markwatson.com/" :obey-robots t)

;; Fetch and return all href links
(fetch-and-extract-links "https://markwatson.com/")

;; Print HTML snippet + links (smoke test)
(demo-fetch "https://markwatson.com/")
```

## API

| Function | Description |
|----------|-------------|
| `(fetch-url url &key dump log-level obey-robots)` | Fetch JS-rendered content; `:dump` defaults to `"html"` |
| `(fetch-and-extract-links url)` | Fetch URL and return list of `href` link strings |
| `(demo-fetch url)` | Print HTML snippet and links to stdout |

`*lightpanda-binary*` — path/name of the lightpanda binary (default: `"lightpanda"`).

## Environment

```bash
export LIGHTPANDA_DISABLE_TELEMETRY=true
```

## License

Apache-2.0
