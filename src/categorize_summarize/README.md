# Text Categorization and Summarization

**Book Chapter:** [Using Local LLMs With Ollama](https://leanpub.com/read/lovinglisp/using-local-llms-with-ollama) — *Loving Common Lisp* (free to read online).

A pure Common Lisp library for automatically categorizing and summarizing text. It uses dictionary-based category scoring (no external API calls or ML models required) and extractive summarization to identify the most important sentences. Categories include topics like politics, finance, sports, health, and more.

This code originated in the 1990s and demonstrates a classic, non-neural approach to NLP that remains useful for fast, offline text analysis.

## Prerequisites

- **SBCL** with [Quicklisp](https://www.quicklisp.org/)
- The `myutils` and `fasttag` libraries (sibling directories in this repository)

## Dependencies

- `myutils`, `fasttag`

## Usage

```lisp
(ql:quickload "categorize_summarize")

(defvar s1 "Plunging European stocks, wobbly bonds and grave concerns about
the health of Portuguese lender Banco Espirito Santo SA made last
week feel like a rerun of the euro crisis.")

(defvar some-words (myutils:words-from-string s1))

;; Categorize the text
(defvar some-categories (categorize_summarize:categorize some-words))

;; Summarize the text (returns the most important sentences)
(categorize_summarize:summarize some-words some-categories)
```

## Available Functions

- `(categorize_summarize:categorize words)` — Assign category tags to a word list and return scored categories.
- `(categorize_summarize:summarize words categories)` — Extract a summary from the text based on category relevance.
- `(categorize_summarize:get-cat-tag-names)` — Return the list of available category tag names.

## Data Files

The `linguistic_data/` subdirectory contains the category-scoring data tables loaded at initialization time.
