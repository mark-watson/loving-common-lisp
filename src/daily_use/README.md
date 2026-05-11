# daily-use — Gemini REPL with Search & Cache

An interactive command-line tool built with SBCL that provides a readline-enabled REPL for querying Google's Gemini API (`gemini-3.1-flash-preview`), with Google Search grounding and a persistent SQLite cache for building LLM context.

## Prerequisites

- **SBCL** with Quicklisp installed
- **GNU readline** — `brew install readline` (macOS)
- **GOOGLE_API_KEY** environment variable set

## Quick Start

```bash
export GOOGLE_API_KEY=your-key-here
sbcl --load run.lisp
```

Or use the Makefile:

```bash
make run
```

## REPL Commands

| Input | Action |
|-------|--------|
| `<text>` | Ask Gemini a question |
| `!<text>` | Ask with Google Search grounding |
| `>` | Add last answer to persistent cache |
| `!` | Clear cache entries older than 1 week |
| `h` / `help` | Show help |
| `q` / `quit` | Exit |
| `Ctrl-D` | Exit |

## How It Works

- **Cache as context**: Cached entries relevant to your current query (matched by bag-of-words keyword overlap) are prepended to each prompt, giving Gemini targeted context from previous conversations.
- **Search grounding**: Prefix a query with `!` to enable Google Search, useful for current events or factual lookups.
- **Line editing**: Full GNU readline support — arrow keys, history, Ctrl-R search, etc.

## Dependencies

- [`gemini`](../gemini/) — Common Lisp Gemini API wrapper (local)
- [`cache-engine`](../cache_engine/) — SQLite-backed persistent cache (local)
- `cl-readline` — GNU readline bindings (Quicklisp)
- `cl-json` — JSON encoding/decoding (Quicklisp)

## Example run showing search, caching, then same query without search

```
$ make run
sbcl --load run.lisp
This is SBCL 2.5.10, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.

"M1 hack: (pushnew :cl+ssl-foreign-libs-already-loaded *features*)" To load "cffi":
  Load 1 ASDF system:
    cffi
; Loading "cffi"
...........

  Gemini Daily-Use REPL  (type 'h' for help)

gemini> h

  Gemini Daily-Use REPL
  ─────────────────────────────────────────
  <text>         Ask Gemini a question
  !<text>        Ask with Google Search grounding
  >              Add last answer to cache
  !              Clear cache entries older than 1 week
  h / help       Show this help
  q / quit       Exit
  Ctrl-D         Exit
  ─────────────────────────────────────────
  Model: gemini-3.1-flash-lite
  Cache: /Users/markwatson/.daily-use-cache.db (0 items)

gemini> !what sci-fi movies are playing today in Flagstaff AZ?
  [Searching...]

For today, Monday, May 11, 2026, the following science fiction movie is playing in Flagstaff, AZ:

*   **Project Hail Mary** (PG-13) is showing at the **Harkins Flagstaff 16**.

Please check the [Harkins Theatres website](https://www.harkins.com) or your preferred ticketing platform to confirm specific showtimes, as they can change throughout the day.

gemini> >
  [Cached. 1 items total]
gemini> what sci-fi movies are playing today in Flagstaff AZ?
  [Thinking...]

For today, Monday, May 11, 2026, the science fiction movie **Project Hail Mary** (PG-13) is playing at the **Harkins Flagstaff 16**.

Please check the [Harkins Theatres website](https://www.harkins.com) or your preferred ticketing platform to confirm specific showtimes, as they can change throughout the day.

gemini> 
$ make run
sbcl --load run.lisp
This is SBCL 2.5.10, an implementation of ANSI Common Lisp.

  Gemini Daily-Use REPL  (type 'h' for help)

gemini> h

  Gemini Daily-Use REPL
  ─────────────────────────────────────────
  <text>         Ask Gemini a question
  !<text>        Ask with Google Search grounding
  >              Add last answer to cache
  !              Clear cache entries older than 1 week
  h / help       Show this help
  q / quit       Exit
  Ctrl-D         Exit
  ─────────────────────────────────────────
  Model: gemini-3.1-flash-lite
  Cache: /Users/markwatson/.daily-use-cache.db (0 items)

gemini> !what sci-fi movies are playing today in Flagstaff AZ?
  [Searching...]

For today, Monday, May 11, 2026, the following science fiction movie is playing in Flagstaff, AZ:

*   **Project Hail Mary** (PG-13) is showing at the **Harkins Flagstaff 16**.

Please check the [Harkins Theatres website](https://www.harkins.com) or your preferred ticketing platform to confirm specific showtimes, as they can change throughout the day.

gemini> >
  [Cached. 1 items total]
gemini> what sci-fi movies are playing today in Flagstaff AZ?
  [Thinking...]

For today, Monday, May 11, 2026, the science fiction movie **Project Hail Mary** (PG-13) is playing at the **Harkins Flagstaff 16**.

Please check the [Harkins Theatres website](https://www.harkins.com) or your preferred ticketing platform to confirm specific showtimes, as they can change throughout the day.

gemini> 
```

## License

Apache 2.0
