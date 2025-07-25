# Common Lisp library to access Google Gemini LLM APIs

This library provides a Common Lisp interface to Google's Gemini Large Language Models. It supports various models like Gemini 1.5 Pro and Gemini 1.5 Flash, and offers functionalities including text generation, token counting, chat conversations, and streaming responses.

This project was originally forked from a library for Perplexity AI in the book [Loving Common Lisp](https://leanpub.com/lovinglisp) by Mark Watson.

## Setting your GOOGLE_API_KEY API key

Define the `GOOGLE_API_KEY` environment variable with the value of your Google API key. You can obtain an API key from the [Google AI Studio](https://aistudio.google.com/app/apikey).

## Dependencies

This library depends on:
- `uiop`
- `cl-json`
- `dexador`
- `alexandria`


Ensure these are available in your Quicklisp local-projects or via ASDF.

## Usage

Load the library using Quicklisp or ASDF:
```common-lisp
(ql:quickload :gemini)
;; or
(asdf:load-system :gemini)
```

### Basic Generation

To generate text from a prompt:
```common-lisp
(gemini:generate "gemini-1.5-flash-latest" "In one sentence, explain how AI works to a child.")
;; => "AI is like a super smart computer brain that learns from information to answer questions and do tasks."
```
You can use other models like `"gemini-2.0-flash"` as the first argument.

### Counting Tokens

To count the number of tokens a prompt will consume for a specific model:
```common-lisp
(gemini:count-tokens "gemini-2.9-flash" "How many tokens is this sentence?")
;; => 8 (example output, actual may vary)
```

## Available Functions
- `(gemini:generate model-id prompt)`: Generates text from a prompt using the specified model.
- `(gemini:count-tokens model-id prompt)`: Counts the tokens for a given prompt and model.


## Original Project Information

The original project from which this was adapted can be found in the repository https://github.com/mark-watson/loving-common-lisp. The `Makefile` mentioned in the original README for fetching library examples is specific to that book's comprehensive collection of projects. This current library is a focused adaptation for Gemini.
