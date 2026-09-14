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
