# daily-use — Gemini REPL with Search & Cache

An interactive command-line tool built with SBCL that provides a readline-enabled REPL for querying Google's Gemini API, with Google Search grounding and a persistent SQLite cache for building LLM context.

Ordinary questions go through [`litelm`](../litelm/), the provider-neutral LLM client, using the model string `gemini/gemini-3.1-flash-lite`. The `!` prefix asks the [`gemini`](../gemini/) library for Google Search grounding instead, because Google Search is a Gemini-native tool that the OpenAI-compatible layer `litelm` speaks does not expose.

## Prerequisites

- **SBCL** with Quicklisp installed
- **GNU readline** — `brew install readline` (macOS)
- **`GEMINI_API_KEY` or `GOOGLE_API_KEY`** environment variable set

Search grounding (`!<query>`) additionally requires `GOOGLE_API_KEY`, which is the variable the `gemini` library reads. Without it, plain questions still work and `run.lisp` prints a note.

## Quick Start

```bash
export GEMINI_API_KEY=your-key-here
sbcl --load run.lisp
```

Or use the Makefile:

```bash
make run
```

`run.lisp` resolves the local systems relative to its own location, so it works from any working directory.

## REPL Commands

| Input | Action |
|-------|--------|
| `<text>` | Ask Gemini a question |
| `!<text>` | Ask with Google Search grounding |
| `>` | Add last answer to persistent cache |
| `!` | Clear cache entries older than 1 week |
| `h` / `help` | Show help |
| `q` / `quit` / `exit` | Exit |
| `Ctrl-D` | Exit |

## How It Works

- **Cache as context**: Cached entries relevant to your current query (matched by bag-of-words keyword overlap) are prepended to each prompt, giving Gemini targeted context from previous conversations. Caching the same answer twice refreshes the existing entry rather than storing a duplicate.
- **Search grounding**: Prefix a query with `!` to enable Google Search, useful for current events or factual lookups.
- **Error handling**: `litelm` signals a distinct condition per failure mode, so rate limits, authentication failures, an exceeded context window and other API errors each produce their own readable message. Errors are displayed but never cached, so `>` cannot store a failure as context.
- **Line editing and persistent history**: Full GNU readline support — arrow keys, `Ctrl-R` search, etc. Input history is read from `~/.daily-use-history` on startup and written back on exit, so it survives between sessions.

## Dependencies

| System | Role |
|--------|------|
| [`litelm`](../litelm/) | Provider-neutral chat completions (local) |
| [`gemini`](../gemini/) | Google Search grounding (local) |
| [`cache-engine`](../cache_engine/) | SQLite-backed persistent cache (local) |
| `cl-readline` | GNU readline bindings (Quicklisp) |
| `dexador`, `cl-json`, `alexandria`, `sqlite` | Pulled in transitively (Quicklisp) |

## Example run showing search, caching, then the same query without search

```
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
  Model: gemini/gemini-3.1-flash-lite
  Cache: /Users/markw/.daily-use-cache.db (0 items)

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

gemini> q
Goodbye.
  [Cache closed]
```

## License

Apache 2.0
