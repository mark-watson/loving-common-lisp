# CLSQL SQLite3 Examples for Common Lisp

This directory contains standalone examples demonstrating how to use Common
Lisp with relational databases via CLSQL, configured for SQLite3 only.

The examples are based on the database access chapter of Mark Watson's
"Loving Common Lisp, or the Savvy Programmer's Secret Weapon" book.

## Requirements

- [SBCL](https://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/)
- The CLSQL library and SQLite3 backend installed via Quicklisp:

```lisp
(ql:quickload "clsql")
(ql:quickload "clsql-sqlite3")
```

## Running the examples

### 1. Create the schema

Start SBCL from within this directory, then load the schema definition and
create the articles table:

```lisp
* (load "clsql_create_news_schema.lisp")
* (create-articles-table)
```

This creates a local SQLite3 database named `news.db` and defines the
`articles` view class.

### 2. Write test data

Load the write-to-database example:

```lisp
* (load "clsql_write_to_news.lisp")
```

This inserts a sample article row into `news.db` and exercises an update.

### 3. Read data back

Load the read-from-database example:

```lisp
* (load "clsql_read_from_news.lisp")
```

This queries the `articles` table and prints each record.

## Notes

- All three examples use `localhost`/`news.db`-style connections tuned for SQLite3.
- Loading `clsql_create_news_schema.lisp` is required before running either
  write or read, as it establishes the connection and view class definition.
