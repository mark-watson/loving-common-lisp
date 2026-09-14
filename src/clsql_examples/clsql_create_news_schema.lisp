(ql:quickload :clsql)
(ql:quickload :clsql-sqlite3)

;; SQLite3 connection specification:
;;    (db-name &key init-command).
;; The first argument to **clsql:connect** is a connection
;; specification list:

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
