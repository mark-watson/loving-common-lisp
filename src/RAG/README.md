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

- **cl-json** — JSON encoding/decoding
- **dexador** — HTTP client; transient failures are retried with exponential backoff
- **usocket** — Socket condition classes used for retry detection
- **uiop** — System utilities

All HTTP (embeddings and the Gemini Interactions API used for generation) is implemented inside the system itself via a small `%post-json` helper.

**Environment variable:** `GOOGLE_API_KEY` must be set.

**Models used:**
- `gemini-3-flash-preview` — Default for all agent LLM calls (`*rag-model*`; override per call with `:model`)
- `gemini-embedding-001` — Free-tier embedding model for document/query vectors (`text-embedding-004` was retired from the v1beta API). The API key is sent in the `x-goog-api-key` header, never in the URL.

Set `*embedding-dimension*` to 768 (or 1536) before building or loading a corpus to cut embedding memory and search time by 4x (2x) with little quality loss; the model default is 3072. If you change the embedding model or dimension, re-embed your corpora: `search` signals a dimension-mismatch error rather than silently scoring with truncated vectors.

Embeddings are computed with batched `batchEmbedContents` calls (at most 100 texts per request, the API cap) and memoized in an in-memory cache (`clear-embedding-cache` resets it; `*embedding-cache-cap*` bounds its size). Transient API failures (HTTP 429/5xx, connection errors) are retried with exponential backoff; permanent 4xx errors signal immediately.

## Quick Start

The rag system is self-contained in this directory. Load `project.lisp`
(it registers the local ASDF system and loads everything), or register
`rag.asd` with ASDF yourself:

```lisp
(load "project.lisp")   ; loads the :rag system

;; Run the built-in demo with sample documents
(rag:test)

;; Or use interactively after test returns corpora:
(defvar *corpora* (rag:test))
(rag:interactive-demo *corpora*)
```

After `project.lisp` has been loaded once in a session, `ql:quickload :rag`
works for subsequent reloads.

## API Reference

### `make-corpus (&key name description)` → corpus
Create an empty corpus (document collection).

### `add-document (corpus filepath &key chunk-size)` → count
Read a text file, split it into overlapping chunks, compute embeddings, and store in the corpus. Returns the number of chunks added.

### `query (corpora question &key max-iterations top-k model max-context-chunks)` → string
Ask a question using the full agentic RAG pipeline. `corpora` can be a single corpus or a list of corpora for cross-corpus retrieval.

### `agentic-rag (corpora user-query &key max-iterations top-k model max-context-chunks)` → string
Low-level entry point with full control. `max-iterations` bounds the sufficiency/refinement loop (default 3), `top-k` sets passages retrieved per query (default 3), `model` overrides `*rag-model*`, and `max-context-chunks` caps how many top-scoring passages are sent to the LLM regardless of iteration count (default 8).

### `save-corpus (corpus pathname)` / `load-corpus (pathname)` → corpus
Persist a corpus (chunks and embeddings) to an s-expression file and load it back, avoiding re-embedding (and re-paying API calls) on every run.

### `corpus-chunk-count (corpus)` → integer
Number of chunks stored in a corpus.

### `*rag-verbose*`
When true (default), each agent prints DEBUG tracing of its decisions — useful for following the pipeline in the book's examples. Bind or set to NIL for quiet library use.

### `interactive-demo (corpora)`
Start an interactive REPL for querying loaded corpora.

### `test ()` → corpora
Run the built-in demo: loads sample documents about renewable energy, electric vehicles, and climate science, then runs three progressively harder queries.

## Tests

Offline unit tests (no network access; the LLM and embedding functions are stubbed via `*generate-fn*` / `*embedding-fn*`):

```lisp
(asdf:test-system :rag)
```

Covers chunking boundary cases (including the forward-progress guard), query line parsing (digits in query text survive, list prefixes of any number are stripped), vector math (including the dimension-mismatch error), retrieval ranking, cross-source deduplication, batched query embedding, batch splitting at the 100-text API cap, cache eviction, retry behavior (transient vs permanent errors), sufficiency-verdict parsing, corpus save/load round-trip with corruption checks, and the full agentic pipeline with a stubbed LLM (including the skipped sufficiency call at the last iteration and quiet-mode operation).

## File Structure

| File | Description |
|---|---|
| `rag.asd` | ASDF system definitions (`rag` and `rag/test`) |
| `package.lisp` | Package definition and exports |
| `embeddings.lisp` | Gemini embedding integration: batched API, cache with eviction, retries, `*rag-verbose*` |
| `vector-store.lisp` | In-memory vector store with normalized embeddings, cosine similarity, chunking, corpus persistence with validation |
| `agents.lisp` | Multi-agent pipeline (rewriter, search, sufficiency, synthesis) |
| `rag.lisp` | Top-level API, interactive demo, and test code |
| `tests.lisp` | Offline unit tests (package `rag-tests`) |
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
