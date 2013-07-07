(ql:quickload :clsql)
(ql:quickload :clsql-postgresql)

;; Open connection to database and create CLOS class and database view for table 'articles':
(load "clsql_create_news_schema.lisp")

(defvar *a1*
  (make-instance
    'article
    :uri "http://test.com"
    :title "Trout Season is Open on Oak Creek"
    :text "State Fish and Game announced the opening of trout season"))

(print *a1*)

(clsql:update-records-from-instance *a1*)

(print *a1*)

(setf (slot-value *a1* 'title) "Trout season is open on Oak Creek!!!")
(clsql:update-records-from-instance *a1*)
;; warning: the last statement changes the "id" column in the table

(print *a1*)

(defvar *a2*
  (make-instance
    'article
    :uri "http://example.com"
    :title "Longest day of year"
    :text "The summer solstice is on Friday."))

(clsql:update-records-from-instance *a2*)

