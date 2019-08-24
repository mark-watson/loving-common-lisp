(ql:quickload :clsql)
(ql:quickload :clsql-postgresql)

;; Postgres connection specification: (host db user password &optional port options tty).
;; The first argument to **clsql:connect** is a connection specification list:

(clsql:connect '("localhost" "news" "markw" nil) :database-type :postgresql)

(clsql:def-view-class article ()
  ((id
    :db-kind :key
    :db-constraints (:auto-increment :not-null :unique)
    :type integer
    :initarg :id)
   (uri
    :accessor uri
    :type (string 50)
    :initarg :uri)
   (title
    :accessor title
    :type (string 90)
    :initarg :title)
   (text
    :accessor text
    :type (string 400)
    :nulls-ok t
    :initarg :text)))

(defun create-articles-table ()
  (clsql:create-view-from-class 'article))

