# Agentic RAG in Common Lisp

**Book Chapter:** Agentic RAG Using the Gemini API — *Loving Common Lisp*

An implementation of **Agentic Retrieval-Augmented Generation (RAG)** in Common Lisp, inspired by Google's research on [Unlocking Dependable Responses with Agentic RAG](https://research.google/blog/unlocking-dependable-responses-with-gemini-enterprise-agent-platforms-agentic-rag/).

Unlike traditional "vanilla" RAG which performs a single retrieve-then-generate pass, agentic RAG uses multiple specialized agents that **plan, rewrite queries, assess context sufficiency, and iteratively search** until enough information is gathered to produce a reliable answer.

## Architecture

The system implements a multi-agent pipeline:

```
User Query
    │
    ▼
┌──────────────────┐
│  Query Rewriter   │  Decomposes complex questions into
│  Agent            │  1-3 focused sub-queries
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│  Search Fanout    │  Embeds each sub-query, searches
│  Agent            │  across multiple corpora
└────────┬─────────┘
         │
         ▼
┌──────────────────┐     ┌─────────────────┐
│ Sufficient Context│────►│  Refine Queries  │
│ Agent             │ NO  │  (iterate)       │
└────────┬─────────┘     └────────┬────────┘
         │ YES                     │
         ▼                         │
┌──────────────────┐               │
│ Synthesis Agent   │◄─────────────┘
│ (final answer)    │
└──────────────────┘
```

### Key Innovation: Sufficient Context Agent

The critical difference from standard RAG is the **Sufficient Context Agent**, which acts as a quality-control inspector. After retrieval, it evaluates:

1. **Retrieved snippets** — Are the actual text chunks relevant and informative?
2. **Completeness** — Does the context address ALL parts of the user's question?
3. **Missing pieces** — What specific information is still needed?

If context is insufficient, the system generates refined search queries and iterates (up to a configurable limit).

## Dependencies

- **llm** — Mark Watson's LLM library (provides Gemini client)
- **cl-json** — JSON encoding/decoding
- **uiop** — System utilities

**Environment variable:** `GOOGLE_API_KEY` must be set.

**Models used:**
- `gemini-2.0-flash` — Inexpensive model for all agent LLM calls
- `text-embedding-004` — Free-tier embedding model for document/query vectors

## Quick Start

```lisp
;; Load the system
(ql:quickload :rag)

;; Run the built-in demo with sample documents
(rag:test)

;; Or use interactively after test returns corpora:
(defvar *corpora* (rag:test))
(rag:interactive-demo *corpora*)
```

## API Reference

### `make-corpus (&key name description)` → corpus
Create an empty corpus (document collection).

### `add-document (corpus filepath &key chunk-size)` → count
Read a text file, split it into overlapping chunks, compute embeddings, and store in the corpus. Returns the number of chunks added.

### `query (corpora question)` → string
Ask a question using the full agentic RAG pipeline. `corpora` can be a single corpus or a list of corpora for cross-corpus retrieval.

### `agentic-rag (corpora user-query &key max-iterations top-k)` → string
Low-level entry point with full control over iteration limit and retrieval count.

### `interactive-demo (corpora)`
Start an interactive REPL for querying loaded corpora.

### `test ()` → corpora
Run the built-in demo: loads sample documents about renewable energy, electric vehicles, and climate science, then runs three progressively harder queries.

## File Structure

| File | Description |
|---|---|
| `rag.asd` | ASDF system definition |
| `package.lisp` | Package definition and exports |
| `embeddings.lisp` | Gemini text-embedding-004 integration |
| `vector-store.lisp` | In-memory vector store with cosine similarity |
| `agents.lisp` | Multi-agent pipeline (rewriter, search, sufficiency, synthesis) |
| `rag.lisp` | Top-level API, interactive demo, and test code |
| `data/` | Sample text documents for the demo |

## Example Output

A multi-hop query like *"How does the carbon footprint of manufacturing EV batteries compare to the emissions saved by charging EVs from renewable energy sources?"* requires the system to:

1. **Rewrite** into sub-queries about EV battery manufacturing emissions AND renewable energy charging benefits
2. **Search** across the electric-vehicles corpus AND climate-science corpus
3. **Assess** whether both pieces of information were found
4. **Synthesize** an answer combining facts from multiple sources with citations

## Reference

- [Unlocking Dependable Responses with Agentic RAG](https://research.google/blog/unlocking-dependable-responses-with-gemini-enterprise-agent-platforms-agentic-rag/) — Google Research Blog, June 2026
- [FRAMES Benchmark](https://arxiv.org/abs/2409.12941) — Evaluation dataset for multi-hop RAG
