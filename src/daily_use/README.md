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

- **Cache as context**: All cached entries are prepended to each prompt, giving Gemini relevant context from previous conversations.
- **Search grounding**: Prefix a query with `!` to enable Google Search, useful for current events or factual lookups.
- **Line editing**: Full GNU readline support — arrow keys, history, Ctrl-R search, etc.

## Dependencies

- [`gemini`](../gemini/) — Common Lisp Gemini API wrapper (local)
- [`cache-engine`](../cache_engine/) — SQLite-backed persistent cache (local)
- `cl-readline` — GNU readline bindings (Quicklisp)
- `cl-json` — JSON encoding/decoding (Quicklisp)

## License

Apache 2.0
