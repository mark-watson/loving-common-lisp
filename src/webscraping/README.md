# Web Scraping Examples in Common Lisp

This directory contains simple, self-contained examples of web scraping and content extraction in Common Lisp.

## Files

1. **[html-headers.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/webscraping/html-headers.lisp)**
   - Fetches a web page and prints out the text within `h1` through `h6` header tags.
   - Demonstrates the use of the `clss` CSS-selector library to query parsed HTML nodes.

2. **[page-text.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/webscraping/page-text.lisp)**
   - Fetches a web page and returns cleaned-up, formatting-aware plain text.
   - Ignores non-content boilerplate elements (e.g., `<script>`, `<style>`, `<head>`, `<nav>`, `<header>`, `<footer>`, `<iframe>`, `<noscript>`).
   - Translates block elements (like `<p>`, `<div>`, list items, headers) into appropriate linebreaks, and normalizes consecutive whitespace and newlines using `cl-ppcre`.

3. **[page-markdown.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/webscraping/page-markdown.lisp)**
   - Fetches a web page and recursively translates its HTML elements into markdown.
   - Renders links (`[Text](URL)`), images (`![Alt](URL)`), bold/italic elements (`**bold**`, `*italic*`), lists (`* item`), headers (`#`, `##`, etc.), inline code, and code blocks (` ``` `).
   - Automatically filters out scripts, styles, and other metadata.

## Dependencies

The scripts automatically load the following packages via Quicklisp:
* **`drakma`**: A mature, full-featured HTTP client.
* **`plump`**: A lenient HTML parser.
* **`clss`**: A DOM traversal library using CSS selectors.
* **`cl-ppcre`**: A regular expression engine used for text cleanup.

## How to Run

To run any of the examples, run them from your shell using SBCL (which will load your `~/.sbclrc` containing Quicklisp):

```bash
sbcl --load html-headers.lisp --eval "(sb-ext:exit)"
sbcl --load page-text.lisp --eval "(sb-ext:exit)"
sbcl --load page-markdown.lisp --eval "(sb-ext:exit)"
```
