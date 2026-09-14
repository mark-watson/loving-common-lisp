# Accessing Relational Databases

There are good options for accessing relational databases from Common Lisp. Personally I almost always use Postgres and in the past I used either native foreign client libraries or the socket interface to Postgres. Recently, I decided to switch to [CLSQL](http://clsql.b9.com/) which provides a common interface for accessing Postgres, MySQL, SQLite, and Oracle databases. There are also several recent forks of CLSQL on github. We will use CLSQL in examples in this book. Hopefully while reading the [Chapter on Quicklisp](#quicklisp) you installed CLSQL and the back end for one or more databases that you use for your projects.

For some database applications when I know that I will always use the embedded SQLite database (i.e., that I will never want to switch to Postgres or another database) I will just use the **sqlite** library as I do in chapter **Knowledge Graph Navigator**.

If you have not installed CLSQL yet, then please install it now:

```lisp
(ql:quickload "clsql")
```

You also need to install one or more CLSQL backends, depending on which relational databases you use:

```lisp
(ql:quickload "clsql-postgresql")
(ql:quickload "clsql-mysql")
(ql:quickload "clsql-sqlite3")
```

The rest of this chapter uses SQLite3 for examples. The directory **src/clsql_examples** contains the standalone example files.

While I often prefer hand crafting SQL queries, there seems to be a general movement in software development towards the data mapper or active record design patterns. CLSQL provides Object Relational Mapping (ORM) functionality to CLOS.

You need SQLite3 installed on your system to follow along with the examples. CLSQL with the SQLite3 backend creates a local database file named **news.db** in the current directory when you run the code.

We will use three example programs that you can find in the **src/clsql_examples** directory in the book repository on github:

- clsql_create_news_schema.lisp to create table "articles" in database "news.db"
- clsql_write_to_news.lisp to write test data to table "articles"
- clsql_read_from_news.lisp to read from the table "articles"

The following listing shows the file **src/clsql_examples/clsql_create_news_schema.lisp**:

```lisp
(ql:quickload :clsql)
(ql:quickload :clsql-sqlite3)

;; SQLite3 connection specification:
;;    (db-name &key init-command)
;; Wrap in UNLESS to avoid error on reload:

(unless (clsql:connected-databases)
  (clsql:connect '("news.db")
                 :database-type :sqlite3))

(clsql:def-view-class articles ()
  ((id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :id)
   (uri
    :accessor uri
    :type (string 60)
    :initarg :uri)
   (title
    :accessor title
    :type (string 90)
    :initarg :title)
   (text
    :accessor text
    :type (string 500)
    :nulls-ok t
    :initarg :text)))

(defun create-articles-table ()
  (clsql:create-view-from-class 'articles))
```

In this repl listing, we create the database table "articles" using the function **create-articles-table** that we just defined:

```lisp
->  src git:(master) sbcl
(running SBCL from: /Users/markw/sbcl)
* (load "clsql_create_news_schema.lisp")
* (create-articles-table)
T
*
```

The following listing shows the file **src/clsql_examples/clsql_write_to_news.lisp**:

```lisp
(ql:quickload :clsql)
(ql:quickload :clsql-sqlite3)

;; Open connection to database and create CLOS class and database view
;; for table 'articles':
(load "clsql_create_news_schema.lisp")

(defvar *a1*
  (make-instance
    'articles
    :uri "http://test.com"
    :title "Trout Season is Open on Oak Creek"
    :text "State Fish and Game announced the opening of trout season"))

(clsql:update-records-from-instance *a1*)
;; modify a slot value and update database:
(setf (slot-value *a1* 'title) "Trout season is open on Oak Creek!!!")
(clsql:update-records-from-instance *a1*)
;; warning: the last statement changes the "id" column in the table
```


You should load the file **clsql_write_to_news.lisp** one time in a repl to create the test data. The following listing shows file **clsql_read_from_news.lisp**:

```lisp
(ql:quickload :clsql)
(ql:quickload :clsql-sqlite3)

;; Open connection to database and create CLOS class and database view
;; for table 'articles':
(load "clsql_create_news_schema.lisp")

(defun pp-article (article)
  (format t
    "~%URI: ~S ~%Title: ~S ~%Text: ~S ~%"
    (slot-value article 'uri)
    (slot-value article 'title)
    (slot-value article 'text)))

(dolist (a (clsql:select 'articles))
  (pp-article (car a)))
```

Here is a full session showing creation of the schema, writing test data, and reading records back:

```lisp
$ sbcl
This is SBCL 2.6.6, an implementation of ANSI Common Lisp.
#P"/Users/markw/quicklisp/setup.lisp"
"starting up quicklisp"

* (ql:quickload "clsql")
* (ql:quickload "clsql-sqlite3")
* (load "clsql_create_news_schema.lisp")
* (create-articles-table)
* (load "clsql_write_to_news.lisp")
* (load "clsql_read_from_news.lisp")

URI: "http://test.com"
Title: "Trout season is open on Oak Creek!!!"
Text: "State Fish and Game announced the opening of trout season"
T
*
```

You can also embed SQL where clauses in queries:

```lisp
(dolist (a (clsql:select 'articles :where "title like '%season%'"))
  (pp-article (car a)))
```

which produces this output:

```
URI: "http://test.com"
Title: "Trout season is open on Oak Creek!!!"
Text: "State Fish and Game announced the opening of
       trout season"
```

In this example, I am using a SQL **like** expression to perform partial text matching.

## Database Wrap Up

You learned the basics for accessing relational databases. When I am designing new systems for processing data I like to think of my Common Lisp code as being purely functional: my Lisp functions accept arguments that they do not modify and return results. I like to avoid side effects, that is changing global state. When I do have to handle mutable state (or data) I prefer storing mutable state in an external database. I use this same approach when I use the Haskell functional programming language.
