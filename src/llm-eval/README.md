# llm-eval

`llm-eval` is a Common Lisp evaluation harness for LLM systems. It loads golden datasets, scores answers with deterministic metrics and an LLM judge, stores runs in SQLite, and finds regressions between runs.

## Requirements

- Common Lisp (tested with SBCL) and Quicklisp
- Local ASDF systems from this repository:
  - `:litelm` (LLM client)
  - `:rag` (needed for `:llm-eval/rag` and `:llm-eval/tests`)
- Quicklisp libraries: `:sqlite` and `:fiveam`
- For live RAG evaluations only, Ollama running locally with:
  - `nomic-embed-text`
  - `llama3.2:3b`
  - `qwen3.5:4b`

Make sure ASDF or Quicklisp can find `:litelm`, `:rag`, and `:llm-eval` (for example, symlink their directories into `~/quicklisp/local-projects/` or add them to your ASDF source registry).

## Quick Start with Make

Run the offline unit tests (no model or network calls):

```sh
make test
```

Compile and load both systems:

```sh
make compile
```

Run the full RAG evaluation example (three runs plus run summaries and comparisons; requires Ollama):

```sh
make examples
```

Run one labeled RAG evaluation and store it in `eval-runs.db`:

```sh
make rag-eval LABEL=my-experiment
```

Print all stored run summaries:

```sh
make summaries
```

Remove `eval-runs.db` and local `.fasl` files:

```sh
make clean
```

## Running from a Lisp REPL

### Offline Unit Tests

```lisp
(ql:quickload :llm-eval/tests)
(asdf:test-system :llm-eval)
```

Every test fakes the model calls.

### Evaluate Your Own System

Pass any function of one argument (a question string returning an answer string) to `run-eval`:

```lisp
(ql:quickload :llm-eval)

(let* ((cases (llm-eval:load-cases "data/rag-golden.lisp"))
       (my-system (lambda (question)
                    ;; Call your LLM or RAG pipeline here.
                    (declare (ignore question))
                    "About 280 ppm."))
       (results (llm-eval:run-eval my-system cases :use-judge nil)))
  (llm-eval:print-report results))
```

Set `:use-judge t` (the default) to grade each answer with `*judge-model*` (defaults to `"ollama/qwen3.5:4b"`).

### Store and Compare Runs

```lisp
(let ((db (llm-eval:open-store "eval-runs.db")))
  (unwind-protect
       (progn
         (llm-eval:print-run-summaries db)
         (llm-eval:print-comparison db 1 2 :metric "judge" :threshold 0))
    (sqlite:disconnect db)))
```

`compare-runs` and `print-comparison` accept `"exact"`, `"f1"`, `"keywords"`, or `"judge"` as `:metric`.

### Live RAG Evaluation

```lisp
(ql:quickload :llm-eval/rag)
(llm-eval::run-rag-eval "baseline")
```

Or run `examples.lisp` to execute three configurations and compare them:

```lisp
(ql:quickload :llm-eval/rag)
(load "examples.lisp")
```

## Project Layout

| File | Purpose |
| --- | --- |
| `llm-eval.asd` | ASDF systems: `:llm-eval`, `:llm-eval/rag`, and `:llm-eval/tests` |
| `package.lisp` | Package definition and exported symbols for `#:llm-eval` |
| `dataset.lisp` | `eval-case` struct and `load-cases` reader (`*read-eval*` is `nil`) |
| `scoring.lisp` | Model-free metrics: `tokens`, `exact-match`, `token-f1`, `keyword-recall` |
| `judge.lisp` | LLM-as-judge prompt, reply parser, `*judge-model*`, and `*judge-function*` |
| `harness.lisp` | `eval-result` struct, `run-eval`, `summarize`, and `print-report` |
| `store.lisp` | SQLite storage (`open-store`, `save-run`, `run-summaries`, `compare-runs`) |
| `rag-eval.lisp` | Connects the harness to `:rag` and `data/rag-golden.lisp` |
| `examples.lisp` | Runs three RAG evaluations and prints summaries and comparisons |
| `tests.lisp` | Offline FiveAM test suite (`llm-eval-tests:all-tests`) |
| `data/rag-golden.lisp` | 12 golden Q&A cases built from the documents in `src/RAG/data` |

## Golden Dataset Format

A dataset file holds Lisp plists, one per case:

```lisp
(:id "co2-preindustrial"
 :question "What was the pre-industrial concentration of atmospheric CO2?"
 :reference "About 280 parts per million (ppm)."
 :keywords ("280"))
```

- `:id`, `:question`, and `:reference` must be strings.
- `:id` should be unique and stable across runs so `compare-runs` can pair results.
- `:keywords` is a list of strings that `keyword-recall` matches as whole tokens.
- `load-cases` binds `*read-eval*` to `nil` so `#.` forms in data files cannot run code.

## Scoring Metrics

All metrics return a number from `0` to `1` (or `nil` if the judge fails to return a valid score):

- **Exact match (`exact-match`)**: `1` when answer and reference produce the same normalized token list, else `0`.
- **Token F1 (`token-f1`)**: SQuAD-style multiset token overlap F1 score.
- **Keyword recall (`keyword-recall`)**: Fraction of required keywords found as whole tokens in the answer.
- **LLM judge (`judge`)**: Sends the question, reference answer, and candidate answer to `*judge-model*` at temperature `0` and parses `SCORE: 0|1` and `REASON:`.
