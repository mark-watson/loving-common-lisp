# A Daily-Use Gemini REPL with Search Grounding and Persistent Cache

In this chapter we build an interactive command-line tool that combines Google's Gemini API with optional search grounding and a persistent SQLite cache. The result is a practical daily-driver REPL: you can ask Gemini questions, ground answers in live web search results, and selectively cache useful responses so they become context for future queries. This project ties together three libraries developed earlier in the book — the **litelm** provider-neutral client, the **gemini** client library, and the **cache-engine** SQLite wrapper from the preceding chapter — into a polished `readline-enabled` command-line application.

## Two Ways to Reach Gemini

The book keeps two paths to Gemini, and this project uses both on purpose.

**litelm** is the provider-neutral client. You name the model as a `"provider/model-name"` string, and litelm splits that string, looks up the provider's base URL and API key, speaks the OpenAI-compatible wire format, and hands back a uniform response object. Plain questions go through it, which is why the model in this program reads `"gemini/gemini-3.1-flash-lite"` rather than a bare Gemini model id.

**gemini** talks to the native `generateContent` endpoint, and it exists for the things the compatible layer cannot express. The most important of those is **Google Search grounding**, which works by declaring a `google_search` tool in the request body — a Gemini platform tool, not an OpenAI function tool, so there is no way to ask for it through the compatible schema. Google's own guidance for the compatibility layer is that it suits code that stays within ordinary text workflows and does not need features such as search grounding, which must be wired up separately.

So the split in this chapter is deliberate: ordinary questions go through litelm, and the `!` prefix goes through the gemini library. If you are reaching for token counts, citations, or platform tools, the same rule applies.

## How It Works

The daily-use REPL implements a simple but effective workflow:

1. **Ask a question** — Type a natural language query and Gemini responds using its training data, optional grounding web search, plus any relevant cached context.
2. **Ask with search** — Prefix your query with `!` to enable Google Search grounding, useful for current events or factual lookups.
3. **Cache useful answers** — Type `>` to save the last answer to a persistent SQLite database. When you ask a new question, the tool extracts keywords from your query and retrieves only cached entries that share keyword overlap — so only relevant context is included.
4. **Manage the cache** — Type `!` alone to clear cache entries older than one week.

This cache-as-context pattern is a lightweight alternative to retrieval-augmented generation (RAG). Instead of embedding documents into a vector store, you manually curate a set of useful facts. At query time, bag-of-words matching retrieves only the cached entries relevant to your current question, keeping context focused and avoiding noise.

## Prerequisites

You need SBCL with Quicklisp installed, GNU readline (`brew install readline` on macOS), and an API key in the environment. litelm accepts either `GEMINI_API_KEY` or `GOOGLE_API_KEY`; the gemini library reads `GOOGLE_API_KEY` only, so search grounding needs that one. The bootstrap script checks for both and warns rather than failing outright.

## Project Structure

The project consists of three files: an ASDF system definition, a bootstrap script, and the main application code. It depends on three local libraries (`litelm`, `gemini`, and `cache-engine`) and one Quicklisp library (`cl-readline`).

### daily-use.asd

The system definition names every direct dependency, with a comment saying what each one is for:

```lisp
;;;; daily-use.asd

(asdf:defsystem #:daily-use
  :description "Interactive REPL for Gemini with search grounding and a persistent cache"
  :author "Mark Watson"
  :license "Apache-2.0"
  :depends-on (#:litelm        ; provider-neutral chat completions
               #:gemini        ; Google Search grounding (a Gemini-native tool)
               #:cache-engine  ; SQLite-backed persistent cache
               #:cl-readline)  ; line editing and history
  :components ((:file "daily-use")))
```

### run.lisp

The bootstrap script registers the local system directories, loads all dependencies via Quicklisp, verifies the API key, and launches the REPL:

```lisp
;;;; run.lisp — Bootstrap and launch the daily-use REPL
;;;;
;;;; Usage:  sbcl --load run.lisp   (from this directory, or anywhere)

(require :asdf)

;; Register the local systems, resolved relative to this file rather than to
;; the current directory, so the script works from any working directory.
(let ((here (or *load-truename* *load-pathname*)))
  (push (make-pathname :directory (pathname-directory here))
        asdf:*central-registry*)
  (dolist (sibling '("litelm" "gemini" "cache_engine"))
    (push (uiop:subpathname here (format nil "../~A/" sibling))
          asdf:*central-registry*)))

;; Load dependencies via Quicklisp
(handler-case
    (ql:quickload '(:daily-use) :silent t)
  (error (c)
    (format t "~%Error loading daily-use: ~A~%" c)
    (format t "~%Make sure you have Quicklisp installed and the following libraries available:~%")
    (format t "  - litelm        (local, ../litelm)~%")
    (format t "  - gemini        (local, ../gemini)~%")
    (format t "  - cache-engine  (local, ../cache_engine, needs sqlite)~%")
    (format t "  - cl-readline   (Quicklisp, requires GNU readline on the system)~%")
    (format t "  - dexador, cl-json, alexandria (Quicklisp, pulled in by the above)~%")
    (format t "~%On macOS, ensure readline is installed:  brew install readline~%")
    (uiop:quit 1)))

;; Verify an API key is available. litelm accepts either variable; the gemini
;; library used for search grounding reads GOOGLE_API_KEY only.
(unless (or (uiop:getenv "GEMINI_API_KEY") (uiop:getenv "GOOGLE_API_KEY"))
  (format t "~%Error: neither GEMINI_API_KEY nor GOOGLE_API_KEY is set.~%")
  (format t "Export one before running:  export GEMINI_API_KEY=your-key-here~%")
  (uiop:quit 1))

(when (null (uiop:getenv "GOOGLE_API_KEY"))
  (format t "~%  [Note: GOOGLE_API_KEY is not set, so the !<query> search-grounding~%")
  (format t "   command will fail. Plain questions will still work.]~%~%"))

;; Launch the REPL
(daily-use:main)
(uiop:quit 0)
```

Two details are worth noting. The local systems are registered relative to `*load-truename*` rather than to `(truename "./")`, so the script works from any working directory and not just from `src/daily_use/`. And the API key check distinguishes the two variables: with only `GEMINI_API_KEY` set, plain questions work and the script prints a note that search grounding will not.

## The Main Application

### Package and Configuration

The application defines a single package and its configuration variables:

```lisp
(defpackage #:daily-use
  (:use #:cl)
  (:export #:main))

(in-package #:daily-use)

;;; ---- Configuration ----

(defvar *model* "gemini/gemini-3.1-flash-lite"
  "litelm model string, \"provider/model-name\". The prefix selects the
   provider; the remainder is the model id sent to the API.")

(defvar *system-prompt*
  "You are a concise, helpful assistant. Prefer short, direct answers."
  "System message sent with every plain (non-search) question.")

(defvar *cache-db-path*
  (merge-pathnames ".daily-use-cache.db" (user-homedir-pathname)))

(defvar *history-file*
  (merge-pathnames ".daily-use-history" (user-homedir-pathname))
  "Where cl-readline stores the input history between sessions.")

(defvar *whitespace* '(#\Space #\Tab #\Newline #\Return)
  "Characters trimmed from user input. Newline and return belong here: a
   pasted line can end with one, and an untrimmed newline would otherwise be
   sent to the API as part of the question.")
```

The model is set to `gemini/gemini-3.1-flash-lite` for fast, inexpensive responses suitable for interactive use. The `gemini/` prefix is litelm's provider selector; everything after the slash is the model id that goes to the API. The `gemini-model-id` function later in the file strips that prefix back off for the one code path that bypasses litelm.

`*whitespace*` deserves its own variable because it is used in three places, and getting it wrong is the kind of bug that only shows up when someone pastes text into the prompt. Trimming only spaces and tabs is not enough: a line ending in a newline would send that newline to the API as part of the question, and — as we will see — it would also break the `!` command detection.

The cache database and the readline history file both live in the user's home directory, so they persist across sessions and working directories.

### State and Cache Context

Two dynamic variables track the runtime state:

```lisp
(defvar *cache* nil
  "The cache-engine instance for persisting useful answers.")
(defvar *last-answer* nil
  "The last answer returned by Gemini, available for caching with '>'.")
```

Before looking at the cache builder, we need a way to extract meaningful keywords from the user's query. The `extract-keywords` function splits text into words, strips punctuation and stop words, and returns a list of content-bearing terms:

```lisp
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

(defvar *punctuation*
  '(#\? #\! #\. #\, #\; #\: #\" #\' #\( #\) #\[ #\])
  "Characters stripped from the ends of a word. Note that % and _ are not
   here: they are LIKE metacharacters, and cache-engine escapes them when it
   builds the query, so callers do not have to work around them.")

(defun extract-keywords (text)
  "Extracts meaningful keywords from TEXT by splitting on whitespace,
   downcasing, removing punctuation, and filtering stop words and short words."
  (let* ((downcased (string-downcase text))
         (words (uiop:split-string downcased :separator *whitespace*))
         (cleaned (mapcar (lambda (w) (string-trim *punctuation* w)) words)))
    (remove-if (lambda (w)
                 (or (<= (length w) 2)
                     (member w *stop-words* :test #'string=)))
               cleaned)))
```

For example, the query `"what sci-fi movies are playing today in Flagstaff AZ?"` produces the keyword list `("sci-fi" "movies" "playing" "today" "flagstaff")`. Words shorter than three characters, punctuation, and common stop words are all filtered out.

These keywords do not go straight into a comparison. `build-context-from-cache` hands them to `cache-engine:lookup`, which turns each one into a SQL `LIKE` pattern — and that matters, because `%` and `_` are wildcards in `LIKE`: `%` matches any run of characters, `_` matches any single character. A question like `"is 50% of 200 right?"` would contribute the pattern `%50%%`, which matches nearly every cached entry and drags irrelevant context into the prompt.

The fix for that lives in `cache-engine:lookup`, not here — the `escape-like-pattern` helper described in the preceding chapter is what does it. `extract-keywords` removes ordinary punctuation, and `lookup` escapes `%`, `_` and the escape character itself before building each pattern, adding `ESCAPE '\'` to the query. Escaping belongs at that layer rather than in the caller: the code that builds the `LIKE` pattern is the code that knows it is building one, so every caller gets a literal match for free instead of each one having to remember to sanitize its terms first.

The `build-context-from-cache` function uses these keywords to retrieve only relevant cached entries:

```lisp
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
(defun gemini-model-id ()
  "The bare model id for the native Gemini API — *MODEL* without its
   \"provider/\" prefix, which only litelm needs."
  (let ((slash (position #\/ *model*)))
    (if slash (subseq *model* (1+ slash)) *model*)))

(defun ask-gemini (prompt &key search-p)
  "Sends PROMPT to Gemini, optionally with Google Search grounding, and
   prepends any cached context relevant to it.

   Returns two values: the answer text, and a flag that is true when that
   text is an error message rather than an answer.

   Plain questions go through litelm, which routes on the \"provider/model\"
   prefix and normalises the provider differences. The search-grounded
   variant uses the gemini library instead, because Google Search grounding
   is a Gemini-native tool: the OpenAI-compatible layer does not expose it."
  (let* ((context (build-context-from-cache prompt))
         (full-prompt (concatenate 'string context prompt)))
    (handler-case
        (if search-p
            (values (gemini:generate-with-search full-prompt (gemini-model-id))
                    nil)
            (values (litelm:response-content
                     (litelm:completion *model*
                                        :messages (list (list :system *system-prompt*)
                                                        (list :user full-prompt))
                                        :temperature 0.3
                                        :max-tokens 2048))
                    nil))
      ;; litelm signals a condition per failure mode, so each one gets the
      ;; advice the user actually needs. Order matters: rate-limit-error and
      ;; authentication-error are both subtypes of api-error.
      (litelm:rate-limit-error (c)
        (values (format nil "[Rate limited (HTTP ~A). Wait a moment and ask again.]"
                        (litelm:api-error-status c))
                t))
      (litelm:authentication-error (c)
        (values (format nil "[Authentication failed (HTTP ~A). Check that GEMINI_API_KEY or GOOGLE_API_KEY is valid.]"
                        (litelm:api-error-status c))
                t))
      (litelm:context-window-exceeded-error (c)
        (values (format nil "[Context window exceeded (HTTP ~A). The cached context is too large; try clearing the cache.]"
                        (litelm:api-error-status c))
                t))
      (litelm:api-error (c)
        (values (format nil "[API error (HTTP ~A): ~A]"
                        (litelm:api-error-status c)
                        (litelm:api-error-body c))
                t))
      (litelm:litelm-error (c)
        (values (format nil "[Configuration error: ~A]" c) t))
      ;; Anything else — including network failures from the gemini library —
      ;; must not drop the user into the debugger.
      (error (c)
        (values (format nil "[Error calling the Gemini API: ~A]" c) t)))))
```

The plain path is three lines of litelm: build a message list in litelm's native format, call `litelm:completion` with the model string, and read the text out of the response with `litelm:response-content`. The `(:system ...)` message shows litelm's message format, where each message is a list whose first element is the role. Temperature is set low because this is a question-answering tool, and `:max-tokens` bounds the cost of an accidental essay.

The `handler-case` is where litelm earns its keep for a tool like this. Because litelm maps HTTP failures onto a condition hierarchy, each failure mode gets the advice the user actually needs:

| Condition | When it is signalled |
|---|---|
| `litelm:rate-limit-error` | HTTP 429 — the API is throttling you |
| `litelm:authentication-error` | HTTP 401 or 403 — the key is wrong |
| `litelm:context-window-exceeded-error` | HTTP 400 mentioning the context — the cached context is too long |
| `litelm:api-error` | Any other API failure, with the status and body |
| `litelm:litelm-error` | Configuration problems, such as no key for the provider |

Clause order matters, because `rate-limit-error`, `authentication-error` and `context-window-exceeded-error` are all subtypes of `api-error`, and `api-error` is itself a subtype of `litelm-error`. Listing the specific conditions first — exactly as the `cond` in the REPL lists the specific commands first — means the narrower handler always wins.

`ask-gemini` returns two values: the text, and a flag saying whether that text is an error message. The flag exists so `display-answer` can decline to remember a failure, which is the next function we look at.

### Printing Help and Answers

Two small functions handle output. `print-help` doubles as the status display, since it reports the active model and the cache size:

```lisp
(defun print-help ()
  (format t "~%  Gemini Daily-Use REPL~%")
  (format t "  ─────────────────────────────────────────~%")
  (format t "  <text>         Ask Gemini a question~%")
  (format t "  !<text>        Ask with Google Search grounding~%")
  (format t "  >              Add last answer to cache~%")
  (format t "  !              Clear cache entries older than 1 week~%")
  (format t "  h / help       Show this help~%")
  (format t "  q / quit       Exit~%")
  (format t "  Ctrl-D         Exit~%")
  (format t "  ─────────────────────────────────────────~%")
  (format t "  Model: ~A~%" *model*)
  (format t "  Cache: ~A (~D items)~%~%"
          *cache-db-path* (cache-engine:count-items *cache*)))
```

`display-answer` prints the answer, and remembers it — but only when it is really an answer:

```lisp
(defun display-answer (text error-p)
  "Prints the answer with a visual separator. An error message is shown but
   is deliberately not remembered, so '>' can never cache a failure into the
   persistent context."
  (cond
    ((null text)
     (format t "~%  [No response from Gemini — check model name or API key]~%~%"))
    (error-p
     (format t "~%~A~%~%" text))
    (t
     (format t "~%~A~%~%" text)
     (setf *last-answer* text))))
```

That second return value is doing real work here. Without it, the natural implementation is to set `*last-answer*` to whatever text came back, which includes error strings. Type a question while the network is down, then type `>`, and the error message is written into the persistent database, where it becomes "context from previous conversations" for every future query that shares a keyword with it. Checking `error-p` before remembering the text keeps failures out of the cache.

### The REPL Loop

The heart of the application is `repl-loop`, which uses `cl-readline` for line editing and history. Three small helpers sit alongside it: `clear-old-cache` performs the cache sweep, while `load-history` and `save-history` move the input history in and out of `*history-file*`:

```lisp
(defun clear-old-cache ()
  "Drop cache entries older than a week and report what happened."
  (let ((before (cache-engine:count-items *cache*)))
    (cache-engine:clear-cache-older-one-week *cache*)
    (let ((after (cache-engine:count-items *cache*)))
      (format t "  [Cleared ~D old entries. ~D items remain]~%"
              (- before after) after))))

(defun load-history ()
  "Load previous input history into readline.
   A missing file is normal on the first run, so it is not an error."
  (handler-case
      (when (probe-file *history-file*)
        (rl:read-history (namestring *history-file*)))
    (error (c)
      (format t "  [Could not read history from ~A: ~A]~%" *history-file* c))))

(defun save-history ()
  "Write the input history back to *HISTORY-FILE* so it survives the session."
  (handler-case
      (rl:write-history (namestring *history-file*))
    (error (c)
      (format t "  [Could not write history to ~A: ~A]~%" *history-file* c))))

;;; ---- REPL ----

(defun repl-loop ()
  "Main REPL loop with cl-readline for line editing and history.

   History is loaded from *HISTORY-FILE* on entry and written back on exit,
   inside an unwind-protect so that an error still saves what you typed."
  (format t "~%  Gemini Daily-Use REPL  (type 'h' for help)~%~%")
  (load-history)
  (unwind-protect
       (loop
         (let ((input (rl:readline :prompt "gemini> " :add-history t)))
           ;; Handle EOF (Ctrl-D)
           (when (null input)
             (format t "~%Goodbye.~%")
             (return))

           (let ((trimmed (string-trim *whitespace* input)))
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
                    (let ((action (cache-engine:add_cache *cache* *last-answer*)))
                      (format t "  [~A ~D items total]~%"
                              (if (eq action :refreshed)
                                  "Already cached, timestamp refreshed."
                                  "Cached.")
                              (cache-engine:count-items *cache*)))
                    (format t "  [No answer to cache yet]~%")))

               ;; "!" alone — clear old cache
               ((string= trimmed "!")
                (clear-old-cache))

               ;; "!<query>" — search-grounded question
               ((char= (char trimmed 0) #\!)
                (format t "  [Searching...]~%")
                (finish-output)
                (multiple-value-bind (answer error-p)
                    (ask-gemini (string-trim *whitespace* (subseq trimmed 1))
                                :search-p t)
                  (display-answer answer error-p)))

               ;; Plain question
               (t
                (format t "  [Thinking...]~%")
                (finish-output)
                (multiple-value-bind (answer error-p) (ask-gemini trimmed)
                  (display-answer answer error-p)))))))
    (save-history)))
```

The `cond` dispatch is worth studying. Each branch is a predicate over the trimmed input, and the order encodes the grammar:

- The empty line and the three quit words come first, so they can never be mistaken for questions.
- `h` and `help` are matched before the single-character `!` test.
- `(string= trimmed "!")` — the exact string — is tested **before** `(char= (char trimmed 0) #\!)`. That ordering is what lets one character mean two things: `!` alone clears old cache entries, while `!` followed by text is a search-grounded question. Because the exact-match test runs first, the character test only ever sees `!` with something after it, so `(subseq trimmed 1)` is never empty and the search branch needs no inner guard for the empty case.
- The final clause catches everything else as a plain question.

This is also why `*whitespace*` includes `#\Newline` and `#\Return`. Trimming only spaces and tabs would leave `"!\n"` as the trimmed input, which is not equal to `"!"`; it would fall through to the character test, be treated as a search query, and send a newline to the API. Trimming all four whitespace characters up front makes `"!\n"` clear the cache like any other bare `!`.

Pressing `>` reports which of the two things `add_cache` did. A new answer prints `[Cached. N items total]`; one that is already in the database prints `[Already cached, timestamp refreshed. N items total]`. The second case matters more than it looks — the refresh moves the entry's timestamp forward, so an answer you keep re-saving never ages out of the one-week sweep. The cache ends up holding what you actually use, rather than what you happened to save once.

The history handling is what makes the tool pleasant on the second day. `repl-loop` calls `load-history` before the first prompt, so the up-arrow reaches yesterday's questions, and `save-history` on the way out. That save sits in an `unwind-protect` cleanup rather than after the loop, so quitting with `q`, hitting `Ctrl-D`, or tripping over an unexpected error all keep what you typed. Both helpers tolerate a missing or unwritable file: the first run has no history file yet, and that is not worth interrupting the user over.

The `(finish-output)` calls before API queries ensure the status messages (`[Thinking...]`, `[Searching...]`) appear immediately rather than being buffered until after the API response arrives.

### Entry Point

The `main` function initializes the cache engine and wraps the REPL in `unwind-protect` to guarantee cleanup:

```lisp
(defun main ()
  "Initialize cache and start the REPL."
  (setf *cache* (make-instance 'cache-engine:cache-engine
                               :db-path (namestring *cache-db-path*)))
  (setf *last-answer* nil)
  (unwind-protect
       (repl-loop)
    (cache-engine:close-cache *cache*)
    (setf *cache* nil)
    (format t "  [Cache closed]~%")))
```

The `unwind-protect` ensures the SQLite connection is closed even if the user exits with `Ctrl-C` or an unhandled error occurs — essential for a tool that writes to a persistent database. Clearing `*cache*` afterwards leaves no dangling handle pointing at a closed connection.

## Running the Tool

Start the REPL with:

```bash
cd src/daily_use
export GEMINI_API_KEY=your-key-here
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
  Model: gemini/gemini-3.1-flash-lite
  Cache: /Users/markw/.daily-use-cache.db (0 items)

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

gemini> q
Goodbye.
  [Cache closed]
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
| `q` / `quit` / `exit` | Exit |
| `Ctrl-D` | Exit |

## Key Takeaways

1. **Use the right client for the job** — litelm handles ordinary generation with one uniform interface and a `"provider/model-name"` string; the gemini library is still needed for Gemini-native features such as Google Search grounding, because the OpenAI-compatible layer has no way to express them.
2. **Cache as context with relevance filtering** — Selectively caching LLM responses and using bag-of-words keyword matching to retrieve only relevant entries keeps prompts focused. This is a lightweight alternative to vector-based RAG.
3. **A condition hierarchy beats a generic handler** — `handler-case` over litelm's error types turns "rate limited", "bad key", and "context too long" into three different, actionable messages instead of one `ERROR` string.
4. **Never cache a failure** — Returning an error flag alongside the text keeps transient network problems out of a database that is fed back to the model as context.
5. **Deduplicate and refresh, don't accumulate** — `add_cache` refreshes an identical entry instead of inserting a second row, which keeps repeated context out of every future prompt and gives re-caching a useful side effect: the entry's timestamp moves forward, so the answers you keep using are the ones that survive the weekly sweep.
6. **Escape where the query is built** — `%` and `_` are wildcards in SQL `LIKE`, and `cache-engine:lookup` is the layer that knows it is building a `LIKE` pattern, so that is where the escaping belongs. Fixing it there means no caller has to sanitize its search terms.
7. **Persist the small things** — `cl-readline` provides line editing, `Ctrl-R` search and history for free, and writing that history to a file is three more lines. It is the difference between a tool you try once and one you reach for daily.
8. **`unwind-protect`** — Wrapping the REPL ensures the SQLite database connection is closed cleanly, even on unexpected exits, and that the history is saved on the way out.
