(dbi:with-connection (conn :sqlite3 :database-name "test.db")
  (let* ((query (dbi:prepare conn "SELECT sql FROM sqlite_master WHERE type='table' and name='test'"))
         (result (dbi:execute query)))
    (loop for row = (dbi:fetch result)
       while row
       do (format t "meta data: ~A~%" row))))

(defvar *z* nil)

(dbi:with-connection (conn :sqlite3 :database-name "test.db")
  (let* ((query (dbi:prepare conn "SELECT * FROM test"))
         (result (dbi:execute query)))
    (loop for row = (dbi:fetch result)
       while row
       do
         (setf *z* row)
         (format t "select all: ~A~%" row))))

;; * *z*
;; printed value:  (:|id| 1 :|str| "cat stand")
