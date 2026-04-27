# AutoContext — Build Effective LLM Prompts from Large Text Datasets

**Book Chapter:** [AutoContext: Prepare Effective Prompts with Context for LLM Queries](https://leanpub.com/read/lovinglisp/autocontext-prepare-effective-prompts-with-context-for-llm-queries) — *Loving Common Lisp* (free to read online).

This example demonstrates how to build context for one-shot LLM prompts from large text datasets that would not fit in a single context window. It uses a combination of BM25 keyword scoring and deep-learning sentence embeddings to find the most relevant text chunks for a given query. The selected context is then used to construct a focused prompt for an LLM.

## Prerequisites

- **SBCL** with [Quicklisp](https://www.quicklisp.org/)
- Python package manager **uv** (for the embeddings helper script)
- Run SBCL with extra heap: `sbcl --dynamic-space-size 4096`

## First-Time Setup

The Python embeddings script must be run once from the command line to download the transformer model from Hugging Face (this may take a few minutes):

```bash
echo "some text" | uv run generate_embeddings.py
```

## Dependencies

- ASDF system: `autocontext`
- Python: `generate_embeddings.py` (uses a sentence-transformer model via `uv`)

## Usage

```lisp
;; Start SBCL with extra memory
;; sbcl --dynamic-space-size 4096

(ql:quickload :autocontext)
;; See main.lisp for the full workflow
```

## How It Works

1. **Chunking** — Large text files are split into manageable chunks.
2. **BM25 scoring** (`bm25.lisp`) — Chunks are ranked by keyword relevance using the BM25 algorithm.
3. **Embedding similarity** — A Python subprocess generates sentence embeddings via a transformer model, and cosine similarity is used to re-rank chunks.
4. **Context assembly** — The top-ranked chunks are concatenated into a context string that fits within an LLM's token limit.

## Files

| File | Description |
|------|-------------|
| `main.lisp` | Entry point and orchestration |
| `bm25.lisp` | BM25 keyword relevance scoring |
| `generate_embeddings.py` | Python helper for sentence embeddings |
| `package.lisp` | Package definition |
