# A Daily-Use Gemini REPL with Search Grounding and Persistent Cache

In this chapter we build an interactive command-line tool that combines Google's Gemini API with Google Search grounding and a persistent SQLite cache. The result is a practical daily-driver REPL: you can ask Gemini questions, ground answers in live web search results, and selectively cache useful responses so they become context for future queries. This project ties together two libraries developed earlier in the book — the **gemini** client library and the **cache-engine** SQLite wrapper — into a polished readline-enabled command-line application.

**Note: This example was vibe coded with AntiGravity and Claude Opus 4.6.**

## How It Works

The daily-use REPL implements a simple but effective workflow:

1. **Ask a question** — Type a natural language query and Gemini responds using its training data plus any relevant cached context.
2. **Ask with search** — Prefix your query with `!` to enable Google Search grounding, useful for current events or factual lookups.
3. **Cache useful answers** — Type `>` to save the last answer to a persistent SQLite database. When you ask a new question, the tool extracts keywords from your query and retrieves only cached entries that share keyword overlap — so only relevant context is included.
4. **Manage the cache** — Type `!` alone to clear cache entries older than one week.

This cache-as-context pattern is a lightweight alternative to retrieval-augmented generation (RAG). Instead of embedding documents into a vector store, you manually curate a set of useful facts. At query time, bag-of-words matching retrieves only the cached entries relevant to your current question, keeping context focused and avoiding noise.

## Prerequisites

You need SBCL with Quicklisp installed, GNU readline (`brew install readline` on macOS), and a `GOOGLE_API_KEY` environment variable set for the Gemini API.

## Project Structure

The project consists of three files: an ASDF system definition, a bootstrap script, and the main application code. It depends on two local libraries (`gemini` and `cache-engine`) and two Quicklisp libraries (`cl-readline` and `cl-json`).

### daily-use.asd

```lisp
;;;; daily-use.asd

(asdf:defsystem #:daily-use
  :description "Interactive REPL for Gemini AI with search grounding and persistent cache"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:gemini #:cache-engine #:cl-readline #:cl-json)
  :components ((:file "daily-use")))
```

### run.lisp

The bootstrap script registers the local system directories, loads all dependencies via Quicklisp, verifies the API key, and launches the REPL:

```lisp
;;;; run.lisp — Bootstrap and launch the daily-use REPL
;;;;
;;;; Usage:  sbcl --load run.lisp

(require :asdf)

;; Register local systems
(push (truename "./") asdf:*central-registry*)
(push (truename "../gemini/") asdf:*central-registry*)
(push (truename "../cache_engine/") asdf:*central-registry*)

;; Load dependencies via Quicklisp
(handler-case
    (ql:quickload '(:daily-use) :silent t)
  (error (c)
    (format t "~%Error loading daily-use: ~A~%" c)
    (format t "~%Make sure you have Quicklisp installed and the following libraries available:~%")
    (format t "  - cl-json~%")
    (format t "  - cl-readline  (requires GNU readline on the system)~%")
    (format t "  - sqlite~%")
    (format t "~%On macOS, ensure readline is installed:  brew install readline~%")
    (uiop:quit 1)))

;; Verify GOOGLE_API_KEY is set
(unless (uiop:getenv "GOOGLE_API_KEY")
  (format t "~%Error: GOOGLE_API_KEY environment variable is not set.~%")
  (format t "Export it before running:  export GOOGLE_API_KEY=your-key-here~%")
  (uiop:quit 1))

;; Launch the REPL
(daily-use:main)
(uiop:quit 0)
```

Note the use of `handler-case` to provide a helpful error message if dependencies are missing, and the explicit API key check before launching — small touches that make a command-line tool pleasant to use.

## The Main Application

### Package and Configuration

The application defines a single package and two configuration variables:

```lisp
;;;; daily-use.lisp — Interactive Gemini REPL with search grounding and cache
;;;;
;;;; Commands:
;;;;   <text>          Ask Gemini a question (plain, no search)
;;;;   !<text>         Ask Gemini with Google Search grounding
;;;;   >               Add last answer to the persistent cache
;;;;   !               Clear cache entries older than one week
;;;;   h / H / help    Show help
;;;;   q / quit / exit  Exit the REPL
;;;;   Ctrl-D          Exit the REPL

(defpackage #:daily-use
  (:use #:cl)
  (:export #:main))

(in-package #:daily-use)

;;; ---- Configuration ----

(defvar *model* "gemini-3.1-flash-lite")
(defvar *cache-db-path*
  (merge-pathnames ".daily-use-cache.db" (user-homedir-pathname)))
```

The model is set to `gemini-3.1-flash-lite` for fast, inexpensive responses suitable for interactive use. The cache database lives in the user's home directory so it persists across sessions and working directories.

### State and Cache Context

Two dynamic variables track the runtime state:

```lisp
;;; ---- State ----

(defvar *cache* nil
  "The cache-engine instance for persisting useful answers.")
(defvar *last-answer* nil
  "The last answer returned by Gemini, available for caching with '>'.")
```

Before looking at the cache builder, we need a way to extract meaningful keywords from the user's query. The `extract-keywords` function splits text into words, strips punctuation and stop words, and returns a list of content-bearing terms:

```lisp
;;; ---- Keyword extraction ----

(defvar *stop-words*
  '("a" "an" "the" "is" "are" "was" "were" "be" "been" "being"
    "have" "has" "had" "do" "does" "did" "will" "would" "shall" "should"
    "may" "might" "must" "can" "could" "am" "it" "its"
    "in" "on" "at" "to" "for" "of" "with" "by" "from" "as"
    "and" "or" "but" "not" "no" "nor" "so" "yet"
    "this" "that" "these" "those" "what" "which" "who" "whom"
    "i" "me" "my" "we" "our" "you" "your" "he" "she" "they" "them"
    "how" "when" "where" "why" "if" "then" "than" "about")
  "Common English stop words to filter from search queries.")

(defun extract-keywords (text)
  "Extracts meaningful keywords from TEXT by splitting on whitespace,
   downcasing, removing punctuation, and filtering stop words and short words."
  (let* ((downcased (string-downcase text))
         (words (uiop:split-string downcased
                                   :separator '(#\Space #\Tab #\Newline)))
         (cleaned (mapcar (lambda (w)
                            (string-trim '(#\? #\! #\. #\, #\; #\: #\" #\' #\( #\))
                                         w))
                          words)))
    (remove-if (lambda (w)
                 (or (<= (length w) 2)
                     (member w *stop-words* :test #'string=)))
               cleaned)))
```

For example, the query `"what sci-fi movies are playing today in Flagstaff AZ?"` produces the keyword list `("sci-fi" "movies" "playing" "today" "flagstaff" "az")`. Words shorter than three characters, punctuation, and common stop words are all filtered out.

The `build-context-from-cache` function uses these keywords to retrieve only relevant cached entries:

```lisp
;;; ---- Cache context builder ----

(defun build-context-from-cache (query)
  "Retrieves cached items relevant to QUERY and builds a context string.
   Uses bag-of-words matching: extracts keywords from the query and finds
   cached entries containing any of those keywords."
  (let* ((keywords (extract-keywords query))
         (items (when keywords
                  (cache-engine:lookup *cache* keywords
                                       :limit 10 :match-any t))))
    (if items
        (format nil "Use the following context from previous conversations when answering:~%~%~{- ~A~%~}~%---~%~%"
                items)
        "")))
```

The `:match-any t` argument tells the cache engine to use `OR` matching — a cached entry is included if it contains *any* of the query keywords, not all of them. This bag-of-words approach ensures that if you cached a movie-related answer last week and now ask about movies again, that context surfaces. But if you ask about something unrelated — say, a recipe — the movie answer stays out of the prompt.

When relevant cached items are found, the function produces a context preamble like:

```text
Use the following context from previous conversations when answering:

- Project Hail Mary is playing at Harkins Flagstaff 16.

---

```

The `~{- ~A~%~}` format directive iterates over the matched items, printing each as a bulleted line. This context is prepended to the prompt so Gemini can reference previously cached facts without the user repeating them.

### Query Dispatch

The `ask-gemini` function handles both plain and search-grounded queries:

```lisp
;;; ---- Query dispatch ----

(defun ask-gemini (prompt &key search-p)
  "Sends PROMPT to Gemini, optionally with Google Search grounding.
   Prepends relevant cached context to the prompt."
  (let* ((context (build-context-from-cache prompt))
         (full-prompt (concatenate 'string context prompt)))
    (handler-case
        (if search-p
            (gemini:generate-with-search full-prompt *model*)
            (gemini:generate full-prompt *model*))
      (error (c)
        (format nil "[Error calling Gemini API: ~A]" c)))))
```

The `handler-case` wrapping is important for a daily-use tool — network errors, rate limits, and API issues should produce a readable message rather than dropping the user into the debugger.

### The REPL Loop

The heart of the application is `repl-loop`, which uses `cl-readline` for line editing and history:

```lisp
;;; ---- REPL ----

(defun repl-loop ()
  "Main REPL loop with cl-readline for line editing and history."
  (format t "~%  Gemini Daily-Use REPL  (type 'h' for help)~%~%")
  (loop
    (let ((input (rl:readline :prompt "gemini> " :add-history t)))
      ;; Handle EOF (Ctrl-D)
      (when (null input)
        (format t "~%Goodbye.~%")
        (return))

      (let ((trimmed (string-trim '(#\Space #\Tab) input)))
        (cond
          ;; Empty line — skip
          ((string= trimmed "")
           nil)

          ;; Quit
          ((member trimmed '("q" "quit" "exit") :test #'string-equal)
           (format t "Goodbye.~%")
           (return))

          ;; Help
          ((member trimmed '("h" "help") :test #'string-equal)
           (print-help))

          ;; ">" — cache last answer
          ((string= trimmed ">")
           (if *last-answer*
               (progn
                 (cache-engine:add_cache *cache* *last-answer*)
                 (format t "  [Cached. ~D items total]~%"
                         (cache-engine:count-items *cache*)))
               (format t "  [No answer to cache yet]~%")))

          ;; "!" alone — clear old cache
          ((string= trimmed "!")
           (let ((before (cache-engine:count-items *cache*)))
             (cache-engine:clear-cache-older-one-week *cache*)
             (let ((after (cache-engine:count-items *cache*)))
               (format t "  [Cleared ~D old entries. ~D items remain]~%"
                       (- before after) after))))

          ;; "!<query>" — search-grounded question
          ((char= (char trimmed 0) #\!)
           (let ((query (string-trim '(#\Space #\Tab) (subseq trimmed 1))))
             (if (string= query "")
                 ;; Edge case: "! " with only whitespace after
                 (let ((before (cache-engine:count-items *cache*)))
                   (cache-engine:clear-cache-older-one-week *cache*)
                   (let ((after (cache-engine:count-items *cache*)))
                     (format t "  [Cleared ~D old entries. ~D items remain]~%"
                             (- before after) after)))
                 (progn
                   (format t "  [Searching...]~%")
                   (finish-output)
                   (display-answer (ask-gemini query :search-p t))))))

          ;; Plain question
          (t
           (format t "  [Thinking...]~%")
           (finish-output)
           (display-answer trimmed)))))))
```

The `cond` dispatch is worth studying. The `!` character serves double duty: alone it clears old cache entries, but followed by text it triggers a search-grounded query. The `(char= (char trimmed 0) #\!)` test catches the `!<query>` case, and a secondary check for an empty query after stripping the `!` handles the edge case of `!` followed by whitespace.

The `(finish-output)` calls before API queries ensure the status messages (`[Thinking...]`, `[Searching...]`) appear immediately rather than being buffered until after the API response arrives.

### Entry Point

The `main` function initializes the cache engine and wraps the REPL in `unwind-protect` to guarantee cleanup:

```lisp
;;; ---- Entry point ----

(defun main ()
  "Initialize cache and start the REPL."
  (setf *cache* (make-instance 'cache-engine:cache-engine
                               :db-path (namestring *cache-db-path*)))
  (setf *last-answer* nil)
  (unwind-protect
       (repl-loop)
    (cache-engine:close-cache *cache*)
    (format t "  [Cache closed]~%")))
```

The `unwind-protect` ensures the SQLite connection is closed even if the user exits with `Ctrl-C` or an unhandled error occurs — essential for a tool that writes to a persistent database.

## Running the Tool

Start the REPL with:

```bash
cd src/daily_use
export GOOGLE_API_KEY=your-key-here
make run
```

## Example Session

The following session demonstrates the search-then-cache workflow. First we ask a question with Google Search grounding (prefix `!`), then cache the answer, then ask the same question without search — Gemini can now answer from the cached context:

```text
$ make run
sbcl --load run.lisp
This is SBCL 2.5.10, an implementation of ANSI Common Lisp.

  Gemini Daily-Use REPL  (type 'h' for help)

gemini> h

  Gemini Daily-Use REPL
  ─────────────────────────────────────────
  <text>         Ask Gemini a question
  !<text>        Ask with Google Search grounding
  >              Add last answer to cache
  !              Clear cache entries older than 1 week
  h / help       Show this help
  q / quit       Exit
  Ctrl-D         Exit
  ─────────────────────────────────────────
  Model: gemini-3.1-flash-lite
  Cache: /Users/markwatson/.daily-use-cache.db (0 items)

gemini> !what sci-fi movies are playing today in Flagstaff AZ?
  [Searching...]

For today, Monday, May 11, 2026, the following science fiction movie is playing
in Flagstaff, AZ:

*   **Project Hail Mary** (PG-13) is showing at the **Harkins Flagstaff 16**.

Please check the Harkins Theatres website or your preferred ticketing platform
to confirm specific showtimes, as they can change throughout the day.

gemini> >
  [Cached. 1 items total]
gemini> what sci-fi movies are playing today in Flagstaff AZ?
  [Thinking...]

For today, Monday, May 11, 2026, the science fiction movie **Project Hail Mary**
(PG-13) is playing at the **Harkins Flagstaff 16**.

Please check the Harkins Theatres website or your preferred ticketing platform
to confirm specific showtimes, as they can change throughout the day.
```

Notice that the second query (without the `!` prefix) produces the same accurate, current answer — even though it did not use Google Search. The keywords `"sci-fi"`, `"movies"`, `"flagstaff"` matched the cached answer, so it was automatically included as context for Gemini.

## REPL Command Reference

| Input | Action |
|-------|--------|
| `<text>` | Ask Gemini a question |
| `!<text>` | Ask with Google Search grounding |
| `>` | Add last answer to persistent cache |
| `!` | Clear cache entries older than 1 week |
| `h` / `help` | Show help |
| `q` / `quit` | Exit |
| `Ctrl-D` | Exit |

## Key Takeaways

1. **Cache as context with relevance filtering** — Selectively caching LLM responses and using bag-of-words keyword matching to retrieve only relevant entries keeps prompts focused. This is a lightweight alternative to vector-based RAG.
2. **Search grounding** — The `!` prefix leverages Google Search through the Gemini API, making the tool useful for current events and factual queries that exceed the model's training cutoff.
3. **GNU readline** — The `cl-readline` library provides full line editing, history, and `Ctrl-R` search out of the box, making the REPL feel like a native shell tool.
4. **`unwind-protect`** — Wrapping the REPL ensures the SQLite database connection is closed cleanly, even on unexpected exits.
5. **Composing libraries** — This tool demonstrates how small, focused Common Lisp libraries (gemini, cache-engine, cl-readline) compose cleanly into a practical application through ASDF and Quicklisp.
