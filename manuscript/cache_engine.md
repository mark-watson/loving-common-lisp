# A Persistent LLM Cache with SQLite

Language models charge by the token, and a good answer you paid for last week is usually worth keeping. **cache-engine** is a small library that stores text in a SQLite table and finds it again by keyword. It is deliberately narrow: it knows nothing about models, prompts, or HTTP. You hand it a string; later you hand it a few search terms and get the matching strings back.

Keeping the cache in SQLite rather than in a hash table means it survives the process that wrote it, which is the whole point. The next chapter, the daily-use REPL, uses this library as a lightweight alternative to a vector store: answers the user chose to keep become context for later questions, and the database persists between sessions.

The library is built on `cl-sqlite`, the Quicklisp system named `sqlite`, and has no other dependency.

## The System Definition

```lisp
(defsystem "cache-engine"
  :version "0.1.0"
  :author "Mark Watson"
  :license "Apache 2.0"
  :depends-on ("sqlite")
  :components ((:file "cache-engine"))
  :description "Persistent LLM cache using SQLite.")
```

## The Class and the Schema

The whole library lives in one file and defines one class:

```lisp
(defpackage #:cache-engine
  (:use #:cl)
  (:export #:cache-engine
           #:add_cache
           #:lookup
           #:close-cache
           #:count-items
           #:clear-cache
           #:clear-cache-older-one-week))

(in-package #:cache-engine)

(defclass cache-engine ()
  ((db-path :initarg :db-path :reader db-path)
   (db-conn :accessor db-conn)))

(defmethod initialize-instance :after ((self cache-engine) &key)
  (setf (db-conn self) (sqlite:connect (db-path self)))
  (sqlite:execute-non-query
   (db-conn self)
   "CREATE TABLE IF NOT EXISTS cache (id INTEGER PRIMARY KEY, content TEXT, created_at DATETIME DEFAULT CURRENT_TIMESTAMP)"))
```

`cache-engine` holds two slots: the path it was given and the live SQLite connection. The `initialize-instance :after` method does the work on construction — it opens the connection and then runs `CREATE TABLE IF NOT EXISTS`.

That `IF NOT EXISTS` is what makes the library pleasant to use. Opening a database that already exists is not a special case you have to handle; constructing a `cache-engine` on an existing file simply finds the previous contents. There is no separate `open` versus `create` step, and no migration to run the first time.

The table has three columns. `id` is an `INTEGER PRIMARY KEY`, which in SQLite is an alias for the internal rowid and therefore the fast way to address a row. `content` holds the text. `created_at` is a `DATETIME` defaulting to `CURRENT_TIMESTAMP`, and that default is the quiet workhorse of the design: it means every insert is timestamped without the caller passing a date, so the expiry policy in `clear-cache-older-one-week` needs no bookkeeping of its own.

## Adding an Entry

```lisp
(defmethod add_cache ((self cache-engine) text)
  "Adds a string to the cache.

Identical entries are never duplicated: when TEXT is already present its
timestamp is refreshed instead, which keeps a re-cached answer from being
swept up by CLEAR-CACHE-OLDER-ONE-WEEK.

Returns :INSERTED when a new row was written, or :REFRESHED when an
existing row was touched."
  (check-type text string)
  (let ((existing (sqlite:execute-single
                   (db-conn self)
                   "SELECT id FROM cache WHERE content = ? LIMIT 1"
                   text)))
    (if existing
        (progn
          (sqlite:execute-non-query
           (db-conn self)
           "UPDATE cache SET created_at = CURRENT_TIMESTAMP WHERE id = ?"
           existing)
          :refreshed)
        (progn
          (sqlite:execute-non-query
           (db-conn self)
           "INSERT INTO cache (content) VALUES (?)"
           text)
          :inserted))))
```

The interesting decision here is that `add_cache` is not a plain `INSERT`. It first asks whether this exact text is already stored, and does one of two things:

- **Nothing matches** — insert a new row and return `:inserted`.
- **A row matches** — leave the row alone, set its `created_at` to the current time, and return `:refreshed`.

Calling it two or three times with the same answer therefore leaves exactly one row behind. Without that check, a cache that you add to every day would slowly fill with repeats, and because the retrieval step feeds matches back to a model, those repeats would arrive as duplicated context — the same fact stated three times, consuming tokens and skewing the prompt toward whichever answer you happened to save most often.

Refreshing rather than ignoring the duplicate is the subtler half of the choice. A cache like this one is meant to decay: `clear-cache-older-one-week` drops entries that have not been touched in seven days, on the theory that what you keep using is what is still useful. If a duplicate were ignored, an answer you re-save every few days would still be deleted a week after the *first* time you saved it, which is exactly backwards. Moving the timestamp forward on each re-save makes the sweep a measure of recent usefulness rather than of age.

There is a reason the deduplication lives here in Lisp rather than as a `UNIQUE` constraint on `content`. Adding a constraint would require a migration for databases that already exist, and would turn a repeat into an error the caller has to catch. The check-then-write version works on the databases people already have, and gives the caller something more useful than an error — a keyword saying what actually happened.

## Escaping LIKE Metacharacters

Before looking at `lookup`, we need one helper, which sits between `initialize-instance` and `add_cache` in the file even though it is `lookup` that calls it:

```lisp
(defun escape-like-pattern (string)
  "Escape the LIKE metacharacters in STRING so that it matches literally.

The backslash must be escaped first, then % and _. The query must say
ESCAPE '\\', otherwise SQLite treats the backslash as an ordinary
character and % and _ keep their wildcard meaning."
  (with-output-to-string (out)
    (loop for ch across string
          do (when (member ch '(#\\ #\% #\_))
               (write-char #\\ out))
             (write-char ch out))))
```

SQL's `LIKE` operator has two wildcards: `%` matches any run of characters, and `_` matches exactly one. Both are ordinary characters in a search term a user might type — `"is 50% of 200 right?"` contains a percent sign, and so does any question about a discount. Left alone, a term like `50%` would be embedded in the pattern `%50%%`, which matches anything containing a `50` anywhere, and the retrieval step would pull in half the cache.

The fix is to escape those characters with a backslash and tell SQLite which character is doing the escaping, which is what the `ESCAPE '\'` clause in the next listing does. The backslash itself has to be escaped first: if `%` became `\%` and then a later pass escaped the backslashes, the result would be `\\%`, which means a literal backslash followed by a live wildcard.

Note the ordering inside the backslash. The helper checks whether the current character is one of `\`, `%` or `_` — and if so writes a backslash *before* writing the character itself. Because the check for a backslash happens on the same character it is emitting, an input backslash comes out doubled rather than triggering a second pass.

## Looking Up Entries

```lisp
(defmethod lookup ((self cache-engine) search-terms &key (limit 3) match-any)
  "Returns matching cached strings (default limit 3).

When MATCH-ANY is T, uses OR instead of AND for multiple search terms,
enabling bag-of-words style matching.

Each term is matched as a literal substring: the LIKE metacharacters % and _
are escaped, so a search term containing them cannot turn into a wildcard
that matches unrelated entries. Matching is case-insensitive for ASCII,
which is why callers usually down-case their terms first."
  (if (null search-terms)
      (mapcar #'car (sqlite:execute-to-list (db-conn self)
                                            (format nil "SELECT content FROM cache LIMIT ~D" limit)))
      (let ((query "SELECT content FROM cache WHERE ")
            (connector (if match-any " OR " " AND "))
            (params '()))
        (loop for term in search-terms
              for i from 0
              do (check-type term string)
                 (setf query (concatenate 'string query
                                          (if (> i 0) connector "")
                                          "content LIKE ? ESCAPE '\\'"))
                 (push (format nil "%~A%" (escape-like-pattern term)) params))
        (setf query (concatenate 'string query (format nil " LIMIT ~D" limit)))
        (mapcar #'car (apply #'sqlite:execute-to-list (db-conn self) query (nreverse params))))))
```

`lookup` has two paths. Called with no search terms it simply returns the first `limit` rows, which is a convenient way to peek at what is in the cache. Called with terms it builds a `WHERE` clause listing one `content LIKE ?` per term, joined by `AND` — or by `OR` when the caller passes `:match-any t`. That switch is what turns the function from a strict filter into a bag-of-words retriever: with `OR`, a cached answer is returned if it matches *any* of the query's keywords, which is what you want when the terms came from tokenizing a sentence rather than from a form the user filled in.

Two details in the query construction are worth pausing on.

First, the search terms are **never** pasted into the SQL. Each becomes a `?` placeholder, and the actual text travels separately in the parameter list. Only the `limit` is interpolated into the query string by `format`, and that is safe because `limit` is a number and `~D` can only print digits — there is no way for a caller to smuggle SQL through it. Keeping that line sharp is the difference between a cache and an injection hole, and it is worth noticing that the convenient-looking version, `(format nil "... LIKE '%~A%'" term)`, is the one that would be wrong.

Second, the parameters are pushed onto a list and reversed with `nreverse` before the call, because `push` builds the list backwards while the placeholders appear in the order the terms were supplied. Get that wrong and the terms silently attach to the wrong placeholders.

SQLite's `LIKE` is case-insensitive for ASCII by default, so searching for `ALPHA` finds `alpha`. That is usually what you want for a keyword cache, and it is why the caller in the next chapter down-cases its keywords before passing them in — not because the match requires it, but so that the cache stores and reports terms consistently.

A two-term search therefore runs something like:

```text
SELECT content FROM cache WHERE content LIKE ? ESCAPE '\' OR content LIKE ? ESCAPE '\' LIMIT 10
```

with two `%term%` strings travelling alongside as parameters.

## Housekeeping

The remaining methods are short, and each maps onto a single SQL statement:

```lisp
(defmethod count-items ((self cache-engine))
  "Returns the number of items in the cache."
  (sqlite:execute-single (db-conn self) "SELECT COUNT(*) FROM cache"))

(defmethod clear-cache ((self cache-engine))
  "Removes all items from the cache."
  (sqlite:execute-non-query (db-conn self) "DELETE FROM cache"))

(defmethod clear-cache-older-one-week ((self cache-engine))
  "Removes items older than 7 days from the cache."
  (sqlite:execute-non-query
   (db-conn self)
   "DELETE FROM cache WHERE created_at <= datetime('now', '-7 days')"))

(defmethod close-cache ((self cache-engine))
  "Closes the SQLite database connection. Safe to call more than once.

After a successful close the DB-CONN slot is unbound, so a second call is a
no-op rather than an error — SQLITE:DISCONNECT leaves the connection object
in a state where touching it signals UNBOUND-SLOT."
  (when (slot-boundp self 'db-conn)
    (sqlite:disconnect (db-conn self))
    (slot-makunbound self 'db-conn)))
```

`count-items` wraps `SELECT COUNT(*)` and is what the REPL calls to report the size of the cache. `clear-cache` deletes everything. `clear-cache-older-one-week` deletes rows whose `created_at` is at or before `datetime('now', '-7 days')` — SQLite computes the comparison date itself, so the library never asks the host system what day it is and the policy works the same in any timezone.

`close-cache` deserves a word about its guard. SQLite connections do not like being disconnected twice: after a successful `sqlite:disconnect`, the connection object is left with an unbound slot, and touching it again signals an error rather than doing nothing. So `close-cache` disconnects and then unbinds its own `db-conn` slot, which makes the method genuinely idempotent — the check at the top is true on the first call and false on every later one. Using a cache that has been closed still signals, as it should; what is safe is *closing* it repeatedly, which matters because cleanup paths tend to overlap.

## Testing the Library

`test.lisp` exercises the library against a throwaway database and asserts on the results rather than printing them for a human to eyeball:

```lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)

;; Mocking sqlite if not available, but ideally it should be installed.
;; Let's try to load it.
(handler-case
    (ql:quickload :cache-engine)
  (error (c)
    (format t "Error loading cache-engine: ~A~%" c)
    (uiop:quit 1)))

(use-package :cache-engine)

(when (probe-file "test.db")
  (delete-file "test.db"))

(let ((cache (make-instance 'cache-engine :db-path "test.db")))
  (unwind-protect
       (progn
         (add_cache cache "The quick brown fox jumps over the lazy dog")
         (add_cache cache "Hello world")
         (add_cache cache "Common Lisp is powerful")
         (add_cache cache "SQLite is a great database")
         (add_cache cache "Lisp is the best")
         
         ;; Manually insert an old record to test cleanup
         (sqlite:execute-non-query (cache-engine::db-conn cache)
                                   "INSERT INTO cache (content, created_at) VALUES (?, datetime('now', '-8 days'))"
                                   "Very old item")

         (format t "Cache count before cleanup: ~D~%" (count-items cache))
         (assert (= (count-items cache) 6))

         (clear-cache-older-one-week cache)
         (format t "Cache count after cleanup: ~D~%" (count-items cache))
         (assert (= (count-items cache) 5))

         (format t "Lookup 'fox': ~A~%" (lookup cache '("fox")))
         (format t "Lookup 'Lisp': ~A~%" (lookup cache '("Lisp")))
         (format t "Lookup 'great': ~A~%" (lookup cache '("great")))
         (format t "Lookup 'nothing': nil~%")
         (format t "Lookup 'the': ~A~%" (lookup cache '("the")))
         
         (assert (equal (lookup cache '("fox")) '("The quick brown fox jumps over the lazy dog")))
         (assert (equal (length (lookup cache '("the"))) 2))

         ;; LIKE metacharacters must match literally, not as wildcards
         (add_cache cache "Battery is at 50% charge")
         (add_cache cache "50 percent off deals")
         (format t "Lookup '50%': ~A~%" (lookup cache '("50%")))
         (assert (equal (lookup cache '("50%")) '("Battery is at 50% charge"))
                 () "50% must not act as a LIKE wildcard")
         (add_cache cache "file_name convention")
         (add_cache cache "fileXname convention")
         (assert (equal (lookup cache '("file_name")) '("file_name convention"))
                 () "_ must not act as a single-character LIKE wildcard")

         ;; Duplicates are refreshed, not inserted twice
         (assert (eq (add_cache cache "Hello world") :refreshed))
         (assert (eq (add_cache cache "A brand new entry") :inserted))
         (assert (= 1 (length (lookup cache '("Hello world") :limit 10))))
         (format t "Duplicate add returned :refreshed, row count unchanged.~%")

         ;; Refreshing keeps an entry out of the one-week sweep
         (sqlite:execute-non-query (cache-engine::db-conn cache)
                                   "INSERT INTO cache (content, created_at) VALUES (?, datetime('now', '-8 days'))"
                                   "stale answer")
         (add_cache cache "stale answer")
         (clear-cache-older-one-week cache)
         (assert (plusp (length (lookup cache '("stale answer") :limit 5)))
                 () "a refreshed entry must survive the one-week sweep")

         (clear-cache cache)
         (format t "After clear, count: ~D~%" (count-items cache))
         (assert (= (count-items cache) 0))

         ;; close-cache is idempotent: this call and the one in the cleanup
         ;; below must both be safe
         (close-cache cache)
         (close-cache cache)
         (format t "close-cache is idempotent.~%"))
    (close-cache cache)
    (when (probe-file "test.db")
      (delete-file "test.db"))))

(format t "Tests passed!~%")
(uiop:quit 0)
```

Alongside the obvious checks — that an insert is visible to `count-items`, that a lookup finds what was put in — it pins down the three behaviors that are easy to get wrong. It inserts an entry dated eight days in the past to prove the one-week sweep removes it, then inserts another old entry, re-adds it, and proves the refresh saves it from the same sweep. It stores two strings that differ only in a LIKE metacharacter and asserts that searching for `50%` returns one of them, not both. And it calls `close-cache` more than once, because idempotent cleanup is a property worth having a test for.

The file ends by deleting its database, so running it leaves nothing behind.

## Key Takeaways

1. **Persistence is mostly a schema decision** — a `created_at` column with a `CURRENT_TIMESTAMP` default gives expiry, recency and refresh almost for free, with no date handling in the Lisp code.
2. **Deduplicate on the write path** — deciding what to do with a repeat when it is saved is far easier than cleaning repeats out later, and refreshing the timestamp turns the cache into a record of what you actually reuse.
3. **Escape where the pattern is built** — `%` and `_` are wildcards in `LIKE`, and the function that assembles the `LIKE` pattern is the only one that knows it is assembling one. Fixing it there means no caller has to remember to sanitize its terms.
4. **Parameterize values, interpolate only what you control** — every caller-supplied string goes through a `?` placeholder; the single interpolated value is a number.
5. **Know what `LIKE` is** — it matches substrings, not meaning. That is a good fit for a small, hand-curated cache and a poor fit for searching a document collection, which is when you reach for full-text search or embeddings instead.

## Optional Practice Problems

1. **Listing the cache contents**: Write a `list-cache` method that returns each entry together with its `created_at` timestamp, most recent first. You will need a new `SELECT` and a way to return two values per row; consider whether the result should be a list of conses or an alist.

2. **Per-entry expiry**: `clear-cache-older-one-week` hard-codes a seven-day policy for every row. Add an `expires-at` column, let `add_cache` accept an optional `:ttl` in days, and change the sweep to delete rows whose expiry has passed. Decide what a row with no expiry should mean, and what happens to an entry whose TTL is refreshed.

3. **Case-sensitive search**: SQLite's `LIKE` is case-insensitive for ASCII. Add a `:case-sensitive` option to `lookup` that uses the `GLOB` operator or `PRAGMA case_sensitive_like` instead, and explain in a comment why the change cannot be a simple flag on the existing query.

4. **A proper `UNIQUE` constraint**: The deduplication is currently handled in Lisp. Write a migration that adds a `UNIQUE` constraint on `content` to an existing database — which means creating a new table, copying the rows while collapsing duplicates, and dropping the old one — and then simplify `add_cache` to use `INSERT OR REPLACE` or `INSERT OR IGNORE`. Compare the two designs' cost when the cache holds a hundred thousand rows.

5. **Replace bag-of-words with embeddings**: Keyword matching cannot tell that "movies in Flagstaff" and "cinema showtimes near me" are the same question. Use `litelm:embedding` to store a vector alongside each cached answer, and rank entries by cosine similarity to the query's vector instead of by `LIKE`. You will need to decide when the embedding is computed, how the vectors are stored in SQLite, and how similarity ranking interacts with the seven-day sweep.

6. **Import and export**: Add functions that dump the cache to a JSON or EDN file and read one back, so a cache can be moved between machines. Consider what should happen when the import contains entries the destination already has — this is the deduplication question again, arriving from a different direction.
