# cache-engine

A simple Common Lisp library for persistent LLM cache using SQLite.

## Installation

This library depends on the `sqlite` library. You can load it using Quicklisp:

```lisp
(ql:quickload :cache-engine)
```

## Usage

The library provides a CLOS class `cache-engine` that manages a SQLite database.

```lisp
(use-package :cache-engine)

;; Create an instance with a path to the database
(defvar *cache* (make-instance 'cache-engine :db-path "llm_cache.db"))

;; Add items to the cache
(add_cache *cache* "The quick brown fox jumps over the lazy dog")
(add_cache *cache* "Common Lisp is powerful")

;; Adding the same text again does not create a second row; it refreshes the
;; entry's timestamp instead, so it is not swept up by clear-cache-older-one-week.
;; Returns :inserted or :refreshed.
(add_cache *cache* "Common Lisp is powerful")   ; => :refreshed

;; Lookup items by search terms
;; Returns 0 to 3 matching strings. Terms match literally — % and _ are escaped.
(lookup *cache* '("fox"))
;; => ("The quick brown fox jumps over the lazy dog")

(lookup *cache* '("Lisp" "powerful"))
;; => ("Common Lisp is powerful")

;; Close the connection when done
(close-cache *cache*)
```

## API

### `(make-instance 'cache-engine :db-path "path/to/db")`
Creates a new cache engine instance.

### `(add_cache engine text)`
Adds a string `text` to the cache.

Identical entries are never duplicated. If `text` is already present its
`created_at` timestamp is refreshed instead, so re-caching an answer that is
still useful keeps it from being removed by `clear-cache-older-one-week`.

Returns `:inserted` when a new row was written, or `:refreshed` when an
existing row was touched.

### `(lookup engine search-terms &key (limit 3) match-any)`
Returns a list of strings from the cache that match the provided `search-terms`.
Defaults to a limit of 3.

Each term is matched as a literal substring. The SQL `LIKE` metacharacters
`%` and `_` are escaped, so a term such as `"50%"` matches only entries
containing that text rather than acting as a wildcard. Matching is
case-insensitive for ASCII, which is why callers usually down-case their
terms first. With `:match-any t`, an entry is returned when it matches *any*
of the terms instead of all of them.

### `(count-items engine)`
Returns the total number of items stored in the cache.

### `(clear-cache engine)`
Deletes all items from the cache.

### `(clear-cache-older-one-week engine)`
Removes items from the cache that were created more than 7 days ago. This relies on a `created_at` timestamp added to each entry automatically.

### `(close-cache engine)`
Closes the SQLite database connection.

## License

Apache 2.0
