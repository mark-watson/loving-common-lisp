# NSK: Neural-Symbolic Knowledge Graph Engine

NSK is a hybrid knowledge graph database written in Common Lisp. It stores
subject-predicate-object triples, answers Datalog-style queries by
unification, and falls back to a local language model through Ollama when a
strict symbolic match is missing.

You use NSK from an interactive REPL, or you run it headless as a small REST
service.

## Features

- **Triplestore** with two hash indices (by subject and by object) for fast
  pattern matching.
- **Durable storage** through an append-only log of s-expressions. NSK replays
  the log on startup to rebuild the graph.
- **Query syntax** as reader macros: `?var` for logic variables, `[s p o]` for
  triple patterns, and `~pred` for a neural relation.
- **Unification** in the Norvig/PAIP style, with conjunctions chained by
  `mapcan`.
- **Neural fallback** to a local Ollama model. When a `~` predicate finds no
  symbolic match, NSK asks the model for the missing object.
- **Text ingestion**: the model turns free text into triples.
- **REST server** on port 8800 with the `--serve` flag.
- **No hard dependencies** in the core. It loads and runs under a bare
  LispWorks or SBCL image.

## Requirements

- Common Lisp: LispWorks 8 (tested) or SBCL.
- Optional, for the neural layer: [Ollama](https://ollama.com) running locally
  with the `qwen3.5:4b` model. NSK reaches it over HTTP with dexador when that
  library is present, or with a native socket on LispWorks.
- Optional, for `--serve`: hunchentoot (loaded on demand through Quicklisp).

The graph engine, the query language, and persistence all work with none of
the optional pieces installed.

## Load it

In a running `lw` image:

```lisp
(load "load.lisp")   ; compiles the source files in memory
(nsk:repl)           ; start the interactive prompt
```

`load.lisp` loads the source files directly, so it needs no ASDF cache on disk.
To load through ASDF instead, put the project on your Quicklisp local-projects
path and run `(ql:quickload :nsk)`.

## Quick start

```
NSK: Neural-Symbolic Knowledge Graph Engine
Type :help for commands, :quit to exit.
nsk> (:add :mark :wrote :nsk)
added :MARK :WROTE :NSK
nsk> (:add :mark :codes-in :lisp)
added :MARK :CODES-IN :LISP
nsk> [?who :wrote :nsk]
who=:MARK
nsk> (ask (?a) [?a :wrote :nsk] [?a :codes-in :lisp])
a=:MARK
nsk> :quit
Bye.
```

## Query syntax

The REPL turns three characters into query forms at read time.

| You type            | Reader produces                          | Meaning                       |
| ------------------- | ---------------------------------------- | ----------------------------- |
| `?person`           | `(logic-var 'person)`                    | a logic variable              |
| `[?p :wrote :nsk]`  | `(match-triple (logic-var 'p) :wrote :nsk)` | one triple pattern         |
| `~:codes-in`        | `(neural-predicate :codes-in)`           | a neural (LLM) relation       |

Combine patterns with `ask`. The first list names the variables to return; each
following clause is a triple pattern. NSK proves the clauses left to right and
threads the bindings forward.

```lisp
(ask (?author ?lang)
  [?author :wrote :nsk]
  [?author ~:codes-in ?lang])
```

If `:codes-in` has a stored value, NSK returns it. If not, and the predicate is
neural (`~`), NSK asks the model.

## Commands

| Command                     | Effect                                    |
| --------------------------- | ----------------------------------------- |
| `:help`                     | show the command list                     |
| `(:add s p o)`              | add a triple                              |
| `(:del s p o)`              | remove a triple                           |
| `(:ingest "text")`          | extract triples from text with the model  |
| `:facts`                    | list every triple                         |
| `:count`                    | show the triple count                     |
| `:save`                     | flush the log to disk                     |
| `:quit`                     | leave the REPL                            |

Any other form is plain Common Lisp, so you can mix queries with normal code.

## Storage

Every `add` and `del` appends one s-expression to the log, then flushes it:

```
(:ADD :MARK :WROTE :NSK)
(:ADD :MARK :CODES-IN :LISP)
(:DEL :MARK :WROTE :NSK)
```

On startup NSK reads the log in order and replays it, so the in-memory graph
matches the last saved state. Replay disables `*read-eval*`, so a log file
cannot run code.

The default log is `nsk-graph.log` in the working directory. Change it with the
`--db` flag or by binding `nsk:*log-path*`.

## Neural layer

NSK talks to Ollama at `http://localhost:11434` and targets the `qwen3.5:4b`
model. Change these by binding `nsk:*ollama-url*` and `nsk:*ollama-model*`.

Inference asks for strict JSON (`{"result": "value"}`) and converts the answer
into a keyword, so `"Common Lisp"` becomes `:COMMON-LISP`. When the daemon is
down, a neural query returns no solutions and prints a short note instead of
failing.

## REST server

Start the server instead of the REPL:

```
nsk --serve --port 8800
```

`POST /query` with a JSON body. A field that is `null` or starts with `?` is a
variable; any other string becomes a keyword.

```
curl -s http://localhost:8800/query \
  -H 'Content-Type: application/json' \
  -d '{"subject": null, "predicate": "wrote", "object": "nsk"}'
```

```json
{"count":1,"results":[{"subject":"mark","predicate":"wrote","object":"nsk"}]}
```

`GET /health` returns the triple count and the model name.

## Build the `nsk` binary

LispWorks (needs a build with delivery):

```
lw -build build.lisp     # writes ./nsk
```

SBCL:

```
sbcl --script build.lisp # writes ./nsk via save-lisp-and-die
```

## Line editing

The REPL reads one form at a time and has no built-in line editor. For history
and arrow keys, wrap it with rlwrap:

```
rlwrap ./nsk
```

## Run the tests

```
echo '(progn (load "load.lisp") (load "tests/tests.lisp"))' | lw
```

The suite covers unification, the store, log replay, the reader macros, the
query engine, JSON, the server helpers, and the neural fallback path (with no
daemon).

## Project layout

```
nsk.asd            system definition (for ASDF/Quicklisp builds)
load.lisp          direct source loader for a running lw image
build.lisp         builds the standalone nsk binary
src/packages.lisp  package definition
src/json.lisp      self-contained JSON reader and writer
src/unify.lisp     logic variables, neural predicates, unification
src/store.lisp     triplestore, indices, log persistence
src/reader.lisp    ?var, [triple], ~neural reader macros
src/neural.lisp    Ollama client, inference, text-to-triples
src/query.lisp     pattern matching, neural fallback, the ask macro
src/repl.lisp      interactive read-eval-print loop
src/server.lisp    optional hunchentoot REST server
src/main.lisp      command-line entry point
tests/tests.lisp   test suite
MANUAL.md          reference manual
```

## License

Apache-2.0.
