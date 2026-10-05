# llm-reasoning — a Common Lisp reasoning library

A small Common Lisp port of the core abstractions in
[llm-reasoners](https://github.com/maitrix-org/llm-reasoners), with the language
model supplied by the sibling [`litelm`](../litelm) library. It talks to a local
**oMLX** server by default, so no API key is needed.

The Python library splits a reasoning method into three pieces; so does this one:

| piece | responsibility | protocol |
|---|---|---|
| **LanguageModel** | talk to the model | `generate` |
| **WorldModel** | state, dynamics, terminal test | `init-state`, `step`, `is-terminal` |
| **SearchConfig** | which actions to try, and how good they are | `get-actions`, `fast-reward`, `reward` |

A **SearchAlgorithm** (`beam-search`) explores the world model using the search
config, and a **Reasoner** ties the three together. Chain-of-thought needs no
search at all, so it is a separate **`cot-reasoner`** — mirroring the Python
repo, where CoT is a plain class rather than a `Reasoner`.

## Files

| file | what it is |
|---|---|
| `llm-reasoning-lib.lisp` | the library: protocol, litelm backend, beam search, CoT, answer handling, offline self-tests |
| `CoT-gsm8k-example.lisp` | worked example: chain-of-thought over GSM8K, answered by a local oMLX model |

There is deliberately **no `.asd` file** — both files are loaded by path. The
library bootstraps Quicklisp and locates `litelm.asd` itself, so it works under
`sbcl --script` as well as interactively.

## Requirements

- **SBCL** (verified on 2.6.8) — `brew install sbcl`
- **Quicklisp** in `~/quicklisp`, with **litelm** loadable as a local project
  (litelm pulls in `dexador`, which Quicklisp installs). If litelm lives
  somewhere unusual, point `LITELM_ASD` at its `.asd` file.
- **oMLX** running and serving a model — check with
  `curl -s http://127.0.0.1:8000/v1/models`

No API key is needed: this oMLX instance accepts unauthenticated requests, and
litelm only sends a bearer token when `OMLX_API_KEY` is set.

The offline self-tests need **none** of the above beyond SBCL and litelm.

## Quick start

From this directory:

```bash
# chain-of-thought on the first 6 GSM8K questions
sbcl --script CoT-gsm8k-example.lisp

# first 2 questions only, then 5-sample self-consistency
sbcl --script CoT-gsm8k-example.lisp 2
sbcl --script CoT-gsm8k-example.lisp 6 5
```

Verified output (57 s wall clock for all six, including the litelm compile):

```
model:     omlx/Laguna-XS-2.1-6bit
questions: 6
samples:   1
...
accuracy: 1.0000 (6/6)
```

Check the library on its own — these 20 checks need **no model and no server**:

```bash
sbcl --non-interactive --load llm-reasoning-lib.lisp \
     --eval '(llm-reasoning:run-self-tests)'
# 20/20 checks passed
```

`run-self-tests` returns `T` on success and signals an error on failure, so it
doubles as a CI check.

## The library

Package `llm-reasoning`, 55 exported symbols.

### Protocol

| generic | signature | notes |
|---|---|---|
| `generate` | `(model prompt &key max-tokens temperature)` | returns the completion as a string |
| `model-name` | `(model)` | human-readable id |
| `init-state` | `(world-model)` | the initial state |
| `step` | `(world-model state action)` | returns `(values next-state aux-plist)`; `aux` may carry `:confidence` for the default `reward` |
| `is-terminal` | `(world-model state)` | ends the search |
| `update-example` | `(component example prompt)` | binds the problem onto world model / search config |
| `get-actions` | `(search-config state)` | candidate actions |
| `fast-reward` | `(search-config state action)` | cheap action score |
| `reward` | `(search-config state action &key r-useful confidence)` | full score; the default method is a weighted geometric mean of usefulness and confidence, controlled by `search-config-reward-alpha` (default 0.5) |
| `search` | `(algorithm world-model search-config)` | returns the best `search-node` |
| `solve` | `(reasoner example &key prompt)` | binds the example, then searches |
| `cot-solve` | `(cot-reasoner question)` | returns `(values answer completions)` |

`world-model` and `search-config` both inherit from `reasoning-component`, which
holds the per-problem `component-example` and `component-prompt`.

### Constructors and defaults

```lisp
(llm-reasoning:make-litelm-model &key model max-tokens temperature retries)
(llm-reasoning:make-beam-search   &key beam-size max-depth verbose)
(llm-reasoning:make-reasoner      &key world-model search-config search-algorithm)
(llm-reasoning:make-cot-reasoner  &key model few-shot instruction n-samples temperature)
```

| constructor | defaults |
|---|---|
| `make-litelm-model` | `model` `NIL`, `max-tokens` 512, `temperature` 0.0, `retries` 3 |
| `make-beam-search` | `beam-size` 3, `max-depth` 4, `verbose` `NIL` |
| `make-reasoner` | `search-algorithm` `(make-beam-search)` |
| `make-cot-reasoner` | `n-samples` 1, `temperature` 0.0 |

`make-litelm-model` takes a litelm `"provider/model-name"` string, or `NIL` for
litelm's default (`omlx/Laguna-XS-2.1-6bit`).

### Answer handling

`retrieve-answer` pulls the value after "the answer is" (case-insensitively,
taking the **last** occurrence of the phrase and the first number after it),
`answer-from-dataset` takes what follows `####`, `normalize-answer` drops `$`,
commas, spaces and a trailing period, `answer-equal` compares numerically when
both sides parse as numbers, `majority-vote` powers self-consistency (ties broken
by first appearance), and `accuracy` scores predictions against golds.

All of these accept a string, symbol, character **or number** — GSM8K golds read
from JSON often arrive as numbers, and they compare correctly against the
string answers a model produces.

`retrieve-answer` returns `NIL` when the phrase is absent *or* when nothing
numeric follows it, so an unparseable completion scores as wrong rather than
raising an error.

`*read-eval*` is disabled when parsing answers, because the input is model output.

### Search nodes

`search-node` has `search-node-state`, `search-node-reward` (accumulated) and
`search-node-trace` (the list of actions taken to reach it).

Beam search expands every non-terminal node in the beam, scores each child with
`reward`, keeps the best `beam-size` children, and stops early as soon as any
surviving node is terminal or nothing is left to expand.

## Writing your own reasoning method

Subclass the protocol classes and implement the generics. This complete,
runnable example needs no model (output verified):

```lisp
(load "llm-reasoning-lib.lisp")

(defclass count-to-five-world-model (llm-reasoning:world-model) ())

(defmethod llm-reasoning:init-state ((wm count-to-five-world-model)) 0)

(defmethod llm-reasoning:is-terminal ((wm count-to-five-world-model) state)
  (>= state 5))

(defmethod llm-reasoning:step ((wm count-to-five-world-model) state action)
  (values (+ state action) (list :confidence 1.0)))

(defclass count-to-five-config (llm-reasoning:search-config) ())

(defmethod llm-reasoning:get-actions ((sc count-to-five-config) state)
  (if (>= state 5) '() '(1 2)))

(defmethod llm-reasoning:fast-reward ((sc count-to-five-config) state action)
  (declare (ignore state action))
  1.0)

(defmethod llm-reasoning:reward ((sc count-to-five-config) state action
                                 &key r-useful confidence)
  (declare (ignore r-useful confidence))
  (let ((next (+ state action)))
    (cond ((= next 5) 1.0)
          ((< next 5) (* 0.9 (/ next 5.0)))
          (t 0.0))))

(let ((node (llm-reasoning:solve
             (llm-reasoning:make-reasoner
              :world-model (make-instance 'count-to-five-world-model)
              :search-config (make-instance 'count-to-five-config)
              :search-algorithm (llm-reasoning:make-beam-search
                                 :beam-size 3 :max-depth 5))
             "count to five")))
  (format t "state ~A via ~A~%"
          (llm-reasoning:search-node-state node)
          (llm-reasoning:search-node-trace node)))
;; => state 5 via (2 2 1)
```

To make a **model-driven** world model — the RAP pattern — call the model inside
`step`:

```lisp
(defmethod llm-reasoning:step ((wm my-world-model) state action)
  (let ((text (llm-reasoning:generate (my-world-model-lm wm)
                                      (build-prompt state action)
                                      :temperature 0.0)))
    (values (parse-next-state text)
            (list :confidence 1.0))))
```

## Choosing a model

The model is just a litelm string, so anything litelm can route works:

```lisp
(llm-reasoning:make-litelm-model :model "omlx/Laguna-XS-2.1-6bit")           ; default
(llm-reasoning:make-litelm-model :model "omlx/mlx-community--Qwen3.8-27B-OptiQ-4bit")
(llm-reasoning:make-litelm-model :model "openai/gpt-4o")                     ; needs OPENAI_API_KEY
```

**Prefer `omlx/Laguna-XS-2.1-6bit`.** In an earlier 8-question GSM8K comparison
on this machine it was both the fastest and the most accurate of the three local
candidates (78 s / 77.6 tok/s, versus 56 s / 62.6 tok/s for gemma-4 and 1420 s /
13.7 tok/s for Qwen3.8).

**Do not route gemma-4 through litelm.** That MLX repo ships no `chat_template`,
so oMLX returns an *empty* message (and raw completions just echo the prompt).
The upstream Python repo works around this in its own oMLX backend by applying a
Gemma turn template before sending; litelm has no equivalent hook.

## Gotchas

### dexador's `*PRINT-CASE*` bug (this one is nasty)

litelm POSTs through dexador. dexador's `DEFINE-ALIST-CACHE` macro builds
function names with `(FORMAT NIL "LOOKUP-IN-~A" ...)`, and `FORMAT` follows
`*PRINT-CASE*`. A `(setf *print-case* :downcase)` in `~/.sbclrc` therefore makes
dexador compile a definition named `|LOOKUP-IN-content-encoding-cache|` while the
source reference reads as `LOOKUP-IN-CONTENT-ENCODING-CACHE`:

```
The function dexador.body::lookup-in-content-encoding-cache is undefined.
```

Because it is in the POST path only, **`dex:get` keeps working** and the failure
looks like anything but a name-case bug. Fix a bad cache once with:

```lisp
(let ((*print-case* :upcase)) (asdf:load-system :dexador :force t))
```

This library binds `*PRINT-CASE*` to `:UPCASE` around its own litelm load, so a
fresh compile comes out correct; an already-cached bad fasl still needs the
`force` above.

### `--script` versus `--load`

`sbcl --script file.lisp` **skips `~/.sbclrc`**, so Quicklisp is not loaded and
the project directory that holds `litelm.asd` is not registered. The library's
bootstrap handles both itself, and honours `LITELM_ASD` if litelm lives somewhere
unusual. A side effect worth knowing: `--script` also skips a
`(setf *print-case* :downcase)`, which sidesteps the dexador bug above.

### `STEP` and `SEARCH` are shadowed

`llm-reasoning` shadows the locked `COMMON-LISP` symbols `STEP` (a macro) and
`SEARCH` (a function) so the protocol can keep the names llm-reasoners uses. A
package that `:USE`s both `CL` and `llm-reasoning` must `shadowing-import` them;
`CoT-gsm8k-example.lisp` imports the handful of symbols it needs instead:

```lisp
(defpackage #:my-example
  (:use #:cl)
  (:import-from #:llm-reasoning
                #:make-litelm-model #:make-cot-reasoner #:cot-solve #:accuracy))
```

Inside the library itself, the CL `search` is written `cl:search`.

### Loading the example without running it

`CoT-gsm8k-example.lisp` calls `main` at load time, guarded by
`cot-gsm8k-example:*run-on-load*` (a `defvar`, so a pre-existing value survives
the load). The switch lives in a package the file itself defines, so it has to be
created before the first load:

```lisp
(setf (symbol-value (intern "*RUN-ON-LOAD*"
                            (or (find-package :cot-gsm8k-example)
                                (make-package :cot-gsm8k-example))))
      nil)
(load "CoT-gsm8k-example.lisp")
```

Verified: this loads the example, defines everything, and contacts no model.

### Token budget

Chain-of-thought needs room to finish. With `max-tokens` at 1024 the
150%-profit GSM8K question gets truncated mid-deliberation ("Let me think
again…") and scores as wrong; at the 2048 default it answers correctly. Two
details in the example matter for reproducing the Python accuracy: the
`additional_prompt="ANSWER"` instruction ("Your response need to be ended with
\"So the answer is\"") and that 2048-token budget.

### ASDF cache location

ASDF writes fasls under `~/.cache/common-lisp`. If that path is not writable
(for example when running under a sandbox that only allows the project
directory), point it somewhere inside the project:

```bash
XDG_CACHE_HOME=$PWD/.cl-cache sbcl --script CoT-gsm8k-example.lisp
```

## Results

Re-verified on 2026-10-05 with SBCL 2.6.8 and oMLX serving
`Laguna-XS-2.1-6bit`:

| check | result |
|---|---|
| `run-self-tests` (offline, no model) | **20/20 passed** |
| `compile-file` on both sources | **clean, no warnings** |
| `CoT-gsm8k-example.lisp`, 6 questions | **accuracy 1.0000 (6/6)**, 57 s |
| README "count to five" example | `state 5 via (2 2 1)` as documented |

## Known limitations

- `retrieve-answer` takes the *first* numeric run after "the answer is", not the
  last number in the sentence, so a chain that hedges ("the answer is unclear,
  but 7 apples…") yields `7`. A non-numeric answer yields `NIL`.
- `make-reasoner` requires `:world-model` and `:search-config`; omitting them
  surfaces later as a `no-applicable-method` error on `update-example` rather
  than a clear message at construction time.
- `litelm-model-pause` (the retry backoff, default 1.0 s) is readable and
  settable but is not a `make-litelm-model` keyword.
- `search-config`'s `batch-size` slot is defined and unused.

## Not ported

- Search algorithms other than beam search — no MCTS or DFS/BFS.
- Anything needing token log-probabilities. oMLX exposes none (`logprobs` and
  `top_logprobs` are accepted and silently dropped), so score-candidate
  algorithms such as self-evaluation are not available; use a sampling-based
  reward instead, as the Python RAP example does.
- Datasets and benchmark harnesses. `CoT-gsm8k-example.lisp` embeds the first
  six GSM8K test rows and the four CoT prompt examples directly; extend
  `*gsm8k-examples*` / `*cot-few-shot*` or load a larger file yourself.
- Visualization and logging.
