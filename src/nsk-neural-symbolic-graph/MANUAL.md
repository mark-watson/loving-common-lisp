# NSK Manual

A short reference for the NSK engine. See README.md for an overview.

## Quick start: a worked example

NSK runs from source in a live LispWorks image, with no build step. Start `lw`
in this directory, load the engine, and enter its REPL:

```lisp
(load "load.lisp")
(nsk:repl)
```

You do not need `(in-package :nsk)`. The REPL sets its own package and turns on
the query syntax for you. You will see:

```text
NSK: Neural-Symbolic Knowledge Graph Engine
Type :help for commands, :quit to exit.
nsk>
```

A new graph is empty, so add a few facts before you query. Each `(:add s p o)`
stores one triple:

```text
nsk> (:add :mark :wrote :nsk)
added MARK WROTE NSK
nsk> (:add :jane :wrote :book)
added JANE WROTE BOOK
nsk> (:add :mark :codes-in :lisp)
added MARK CODES-IN LISP
```

List what you stored with `:facts`:

```text
nsk> :facts
  MARK  WROTE  NSK
  JANE  WROTE  BOOK
  MARK  CODES-IN  LISP
(3 triples)
```

Now query. The pattern `[?who :wrote :nsk]` finds every subject that wrote
`:nsk`. Jane wrote `:book`, so only Mark matches:

```text
nsk> [?who :wrote :nsk]
who=:MARK
```

Join two patterns with `ask`. This finds an author of `:nsk` who also codes in
Lisp:

```text
nsk> :
a=:MARK
```

Leave the REPL with `:quit`:

```text
nsk> :quit
Bye.
```

The `[...]`, `?x`, and `~p` syntax works only inside the REPL, which switches it
on. At a plain `lw` prompt these characters are undefined, so reading
`[?who :wrote :nsk]` raises `The variable [?WHO is unbound.` To use the syntax
in your own code, see the next section.

## Using NSK from your own code

Outside the REPL two defaults change. The active graph `*graph*` starts as
`nil`, so bind it first. And the reader syntax stays off until you turn it on
with `enable-nsk-syntax`:

```lisp
(load "load.lisp")
(in-package :nsk)
(setf *graph* (make-graph))   ; or (open-store #p"nsk-graph.log") to load a saved graph
(enable-nsk-syntax)

(add-triple :mark :wrote :nsk)
(add-triple :mark :codes-in :lisp)

(solutions (ask (?a) [?a :wrote :nsk] [?a :codes-in :lisp]))
;; => (((A . :MARK)))
```

Skip the `(setf *graph* ...)` step and the first `add-triple` fails with
`NIL is not of type GRAPH`, because there is no graph to add to.

## Starting NSK

| Goal                     | Command                                              |
| ------------------------ | ---------------------------------------------------- |
| REPL in a running image  | `(load "load.lisp")` then `(nsk:repl)`               |
| REPL from the binary     | `./nsk`                                               |
| REPL with line editing   | `rlwrap ./nsk`                                        |
| REST server              | `./nsk --serve --port 8800`                           |
| Pick a log file          | `./nsk --db mygraph.log`                              |
| Help                     | `./nsk --help`                                        |

## Query syntax

This syntax is active inside the REPL. Outside it, call `enable-nsk-syntax`
first (see "Using NSK from your own code" above).

| Form               | Reads as                                        |
| ------------------ | ----------------------------------------------- |
| `?x`               | `(logic-var 'x)`                                |
| `[s p o]`          | `(match-triple s p o)`                          |
| `~p`               | `(neural-predicate 'p)`                         |

A term is a keyword (`:mark`), a string, or a `?variable`. A predicate may be a
keyword (`:wrote`) or a neural predicate (`~:codes-in`).

### One pattern

```lisp
[?who :wrote :nsk]
```

Returns one solution per matching triple.

### A conjunction

```lisp
(ask (?author ?lang)
  [?author :wrote :nsk]
  [?author ~:codes-in ?lang])
```

`ask` takes a list of result variables and a body of triple patterns. It proves
the patterns left to right, passing bindings forward, and returns the bindings
for the named variables.

## REPL commands

| Command             | Effect                                       |
| ------------------- | -------------------------------------------- |
| `:help`             | list commands                                |
| `(:add s p o)`      | add a triple and log it                      |
| `(:del s p o)`      | remove a triple and log it                   |
| `(:ingest "text")`  | extract triples from text with the model     |
| `:facts`            | print every triple                           |
| `:count`            | print the triple count                       |
| `:save`             | flush the log                                |
| `:quit` / `:exit`   | leave the REPL                               |

Anything else is evaluated as Common Lisp with the NSK reader syntax active.

## REST API

Base URL: `http://localhost:8800`.

### POST /query

Request body (JSON). A field that is `null` or begins with `?` is a variable.

```json
{"subject": null, "predicate": "wrote", "object": "nsk"}
```

Response:

```json
{
  "count": 1,
  "results": [
    {"subject": "mark", "predicate": "wrote", "object": "nsk"}
  ]
}
```

### GET /health

```json
{"status": "ok", "triples": 42, "model": "qwen3.5:4b"}
```

## Public API

Package `nsk`.

### Storage

| Symbol                        | Purpose                                       |
| ----------------------------- | --------------------------------------------- |
| `*log-path*`                  | default log path                              |
| `open-store` `&optional path` | open, replay, and keep the log open           |
| `close-store` `&optional g`   | flush and close the log                       |
| `make-graph`                  | fresh in-memory graph with no log             |
| `*graph*`                     | the active graph                              |
| `add-triple` `s p o &optional g`  | add and log a triple                      |
| `remove-triple` `s p o &optional g` | remove and log a triple                 |
| `all-triples` `&optional g`   | list triples in insertion order               |
| `triple-count` `&optional g`  | number of live triples                        |

### Query

| Symbol                          | Purpose                                     |
| ------------------------------- | ------------------------------------------- |
| `match-triple` `s p o &optional g` | run one pattern, return a query result   |
| `ask` `(vars) &body patterns`   | run a conjunction                           |
| `prove` `patterns env g`        | list of environments that satisfy patterns  |
| `unify` `x y &optional env`     | unify two terms                             |
| `resolve` `x env`               | substitute bound variables                  |
| `solutions` `result`            | raw solution list from a query result       |

### Neural

| Symbol                          | Purpose                                     |
| ------------------------------- | ------------------------------------------- |
| `*ollama-url*`                  | daemon base URL                             |
| `*ollama-model*`                | model name                                  |
| `query-neural-fallback` `s p`   | ask the model for an object                 |
| `text->triples` `text`          | parse text into keyword triples             |
| `ingest-text` `text &optional g`| extract triples and add them                |
| `sanitize-to-keyword` `string`  | `"Common Lisp"` to `:COMMON-LISP`           |

### Server and entry points

| Symbol                          | Purpose                                     |
| ------------------------------- | ------------------------------------------- |
| `start-server` `&optional port` | load hunchentoot and serve                  |
| `stop-server`                   | stop the acceptor                           |
| `repl` `&optional g`            | start the interactive loop                  |
| `main`                          | command-line entry point                    |

## Configuration

Bind these before you start:

```lisp
(setf nsk:*log-path* #p"/data/graph.log")
(setf nsk:*ollama-model* "qwen3.5:4b")
(setf nsk:*ollama-url* "http://localhost:11434")
```
