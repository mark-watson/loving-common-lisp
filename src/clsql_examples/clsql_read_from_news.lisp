(ql:quickload :clsql)
(ql:quickload :clsql-postgresql)

;; Open connection to database and create CLOS class and database view for table 'articles':
(load "clsql_create_news_schema.lisp")

(defun pp-article (article)
  (format t
    "~%URI: ~S ~%Title: ~S ~%Text: ~S ~%"
    (slot-value article 'uri)
    (slot-value article 'title)
    (slot-value article 'text)))

(dolist (a (clsql:select 'article))
  (pp-article (car a)))


(dolist (a (clsql:select 'article :where "title like '%season%'"))
  (pp-article (car a)))

