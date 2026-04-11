# Interfacing with External Programs: A Lightpanda Browser Client

In this chapter, we build a complete Common Lisp library that interfaces with an external program: the Lightpanda headless browser. This example demonstrates practical techniques for shell integration, string processing, and building reusable APIs. Directions for installing the Lightpanda command line tool can be found in the [Lightpanda documentation](https://lightpanda.io/docs/open-source/installation).

## The Problem: JavaScript-Rendered Web Content

Modern web pages often require JavaScript execution to display their content. Traditional HTTP clients like Drakma only fetch static HTML, missing the dynamic content rendered by JavaScript. Lightpanda is a headless browser that runs from the command line and outputs fully rendered pages.

Our goal: create a Common Lisp interface that:
1. Invokes Lightpanda as a subprocess
2. Captures its output (HTML, Markdown, or semantic tree)
3. Provides helper functions for common operations like link extraction

## Project Structure

A well-organized Common Lisp project uses ASDF for system definition:

```lisp
;; lightpanda.asd
(asdf:defsystem #:lightpanda
  :description "Common Lisp interface to the Lightpanda headless browser"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on (#:cl-json)
  :components ((:file "lightpanda")
               (:file "project" :depends-on ("lightpanda"))))
```

Key elements:
- `#:lightpanda` — Uninterned symbol naming the system
- `:depends-on` — External dependencies (loaded via Quicklisp)
- `:components` — Source files in load order

**Package Definition:**

We define a package with explicit exports, creating a clean public API:

```lisp
;; lightpanda.lisp (package section)
(defpackage #:lightpanda
  (:use #:cl)
  (:export
   ;; Config
   #:*lightpanda-binary*
   ;; Fetch interface
   #:fetch-url
   ;; Helpers
   #:fetch-and-extract-links
   #:demo-fetch))

(in-package #:lightpanda)
```

The `:use #:cl` imports the standard Common Lisp symbols. `#:*lightpanda-binary*` and other symbols marked with `#:export` become part of the public API—users call these with package prefixes like `(lightpanda:fetch-url "https://example.com/")`.

## Configuration with Defvar

You must have the `lightpanda` tool installed; here I verify the installation on my laptop:

```
$ which lightpanda
/Users/mark/bin/lightpanda
```

We use `defvar` for configurable settings:

```lisp
(defvar *lightpanda-binary* "lightpanda"
  "Path to the lightpanda binary. Can be an absolute path or a name on PATH.")
```

Using `defvar` (not `defparameter`) means users can `setf` this variable, and it won't be reset if the file is reloaded. The earmuffs (`*...*`) convention marks it as a special variable.

Users can configure a new file path:
```lisp
(setf lightpanda:*lightpanda-binary* "/usr/local/bin/lightpanda")
```

## Running External Programs with UIOP

Common Lisp doesn't have a built-in standard way to run subprocesses, but the UIOP (Universal Interface to OS Processes) library provides portable functions. UIOP comes bundled with ASDF, so it's universally available.

```lisp
(defun %run (command)
  "Run a shell command string, return stdout as a string (or nil on error)."
  (handler-case
      (uiop:run-program command
                        :output :string
                        :error-output :string)
    (error (e)
      (format t "Command error: ~a~%Command: ~a~%" e command)
      nil)))
```

Key UIOP features:
- `:output :string` — Captures stdout as a Lisp string
- `:error-output :string` — Captures stderr separately
- `handler-case` — Catches errors gracefully, returning `nil` on failure

The `%run` name uses the Common Lisp convention: a leading `%` marks internal/private functions not meant for export.

## String Processing: Extracting Links

For link extraction, we scan HTML with simple string operations—a pragmatic choice for this use case:

```lisp
(defun %extract-links (html)
  "Return a list of href strings found in <a> tags within an HTML string."
  (let ((links '())
        (pos    0)
        (marker "href=\""))
    (loop
      (let ((found (search marker html :start2 pos)))
        (unless found (return))
        (let* ((start (+ found (length marker)))
               (end   (position #\" html :start start)))
          (when end
            (push (subseq html start end) links))
          (setf pos (or end (+ found 1)))))))
    (nreverse links)))
```

Walkthrough:
1. `search` finds `"href=\""` starting from position `pos`
2. `subseq` extracts the href value between quotes
3. `push` accumulates links (building a list in reverse)
4. `nreverse` reverses in-place for correct order
5. `loop` (without keywords) is the simple "infinite loop with return" form

The `let*` (not `let`) allows `end` to reference `start`. The `loop` form returns when `found` is `nil`.

## The Main API Function

The central function combines everything:

```lisp
(defun fetch-url (url &key (log-level "warn") obey-robots (dump "html"))
  "Fetch URL using `lightpanda fetch`, returning the JS-rendered content string.
DUMP controls what is written to stdout; valid values are:
  \"html\"               - full rendered HTML (default)
  \"markdown\"           - page as Markdown
  \"semantic_tree\"      - semantic tree
  \"semantic_tree_text\" - semantic tree as plain text
No server process is required; lightpanda is invoked directly.

  (lightpanda:fetch-url \"https://markwatson.com/\")
  (lightpanda:fetch-url \"https://markwatson.com/\" :dump \"markdown\")
"
  (let* ((extra (append (when obey-robots '(\"--obey_robots\"))
                        (list "--dump" dump
                              "--log_level" log-level
                              "--log_format" "pretty")))
         (args  (append (list *lightpanda-binary* "fetch")
                        extra
                        (list url)))
         (cmd   (format nil "~{~a~^ ~}" args)))
    (%run cmd)))
```

Key techniques:
- `&key` with default values: `(dump "html")` makes `:dump` optional
- `when` returns `nil` or the provided list—clean conditional inclusion
- `format nil "~{~a~^ ~}" args` — produces `"arg1 arg2 arg3"` from a list

The `~{~a~^ ~}` format directive:
- `~{` — iterate over a list
- `~a` — aesthetic (human-readable) output
- `~^` — exit iteration if no more elements (no trailing space)
- `~}` — end iteration

## Helper Functions

Higher-level helpers make common operations easy:

```lisp
(defun fetch-and-extract-links (url)
  "Fetch URL with lightpanda and return a list of href link strings.

  (lightpanda:fetch-and-extract-links \"https://markwatson.com/\")
"
  (let ((html (fetch-url url)))
    (if html
        (%extract-links html)
        (progn
          (format t "Failed to fetch ~a~%" url)
          nil))))

(defun demo-fetch (url)
  "Fetch URL, print a snippet of HTML and the discovered links.

  (lightpanda:demo-fetch \"https://markwatson.com/\")
"
  (format t "~%=== Fetch demo: ~a ===~%" url)
  (let ((html (fetch-url url)))
    (if html
        (progn
          (format t "Received ~a bytes of HTML.~%" (length html))
          (format t "First 500 chars:~%~a~%~%" (subseq html 0 (min 500 (length html))))
          (let ((links (%extract-links html)))
            (format t "Found ~a link(s):~%" (length links))
            (dolist (l links)
              (format t "  ~a~%" l))))
        (format t "No HTML returned.~%"))))
```


## Usage Examples

After loading with `(ql:quickload :lightpanda)`:

```lisp
;; Basic HTML fetch
(lightpanda:fetch-url "https://markwatson.com/")

;; Get Markdown output (good for LLM input)
(lightpanda:fetch-url "https://markwatson.com/" :dump "markdown")

;; Respect robots.txt
(lightpanda:fetch-url "https://markwatson.com/" :obey-robots t)

;; Extract all links
(lightpanda:fetch-and-extract-links "https://markwatson.com/")

;; Interactive demo
(lightpanda:demo-fetch "https://markwatson.com/")
```

## Compatibility Package

For users who prefer a different package name, we provide an alias:

```lisp
;; project.lisp
(defpackage #:lightpanda-browser
  (:use #:cl #:lightpanda))

(in-package #:lightpanda-browser)
```

Using `(:use #:cl #:lightpanda)` re-exports all symbols from `lightpanda`, so `(lightpanda-browser:fetch-url ...)` works identically.

## Key Code Style Takeaways

1. **UIOP:run-program** — Portable subprocess execution in Common Lisp
2. **defvar with earmuffs** — Configurable special variables
3. **Package exports** — Design a clean public API
4. **Private function conventions** — Use `%` prefix for internal helpers
5. **Format directives** — `~{~a~^ ~}` for clean command construction
6. **Key arguments with defaults** — `&key (param default)` makes flexible APIs

This pattern—shelling out to a specialized tool and processing its output—is a powerful technique. You can wrap any command-line tool this way: databases, image processors, compilers, or your own scripts. The result is a Lisp API that hides the implementation details while providing access to the tool's capabilities.