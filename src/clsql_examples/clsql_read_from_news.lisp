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
