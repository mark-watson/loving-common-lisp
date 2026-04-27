# FastTag Part-of-Speech Tagger

**Book Chapter:** [Natural Language Processing](https://leanpub.com/read/lovinglisp/natural-language-processing) — *Loving Common Lisp* (free to read online).

A fast, rule-based part-of-speech (POS) tagger implemented in pure Common Lisp. It uses a lexicon loaded from data files combined with transformation rules to assign Penn Treebank POS tags (NN, VB, JJ, DT, etc.) to each word in a sentence. No external APIs or machine learning models are required.

This tagger is a dependency of several NLP libraries in this repository, including `kbnlp` and `categorize_summarize`.

## Prerequisites

- **SBCL** with [Quicklisp](https://www.quicklisp.org/)
- The `myutils` library (sibling directory in this repository)

## Dependencies

- `myutils`

## Usage

```lisp
(ql:quickload "fasttag")

(fasttag:part-of-speech-tagger
  "President Bush went to China. He wanted a good trade agreement.")
;; => #("NNP" "NNP" "VBD" "TO" "NNP" "." "PRP" "VBD" "DT" "JJ" "NN" "NN" ".")
```

## How It Works

1. Each word is looked up in a lexicon hash table loaded from `linguistic_data/FastTagData`.
2. If the word is not found, it is tagged as a noun (NN) by default.
3. A set of transformation rules is applied to correct common tagging errors — for example, a verb following a determiner is re-tagged as a noun.

## Data Files

The `linguistic_data/` subdirectory contains the POS lexicon data (`FastTagData`) loaded at initialization time.

## Available Functions

- `(fasttag:part-of-speech-tagger text)` — Tag each word in the input text (string or array of strings) with its part-of-speech. Returns an array of POS tag strings.
