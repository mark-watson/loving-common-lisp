(defpackage #:cache-engine
  (:use #:cl)
  (:export #:cache-engine
           #:add_cache
           #:lookup
           #:close-cache
           #:count-items
           #:clear-cache
           #:clear-cache-older-one-week))

(in-package #:cache-engine)

(defclass cache-engine ()
  ((db-path :initarg :db-path :reader db-path)
   (db-conn :accessor db-conn)))

(defmethod initialize-instance :after ((self cache-engine) &key)
  (setf (db-conn self) (sqlite:connect (db-path self)))
  (sqlite:execute-non-query
   (db-conn self)
   "CREATE TABLE IF NOT EXISTS cache (id INTEGER PRIMARY KEY, content TEXT, created_at DATETIME DEFAULT CURRENT_TIMESTAMP)"))

(defmethod add_cache ((self cache-engine) text)
  "Adds a string to the cache."
  (sqlite:execute-non-query
   (db-conn self)
   "INSERT INTO cache (content) VALUES (?)"
   text))

(defmethod lookup ((self cache-engine) search-terms &key (limit 3) match-any)
  "Returns matching cached strings (default limit 3).
   When MATCH-ANY is T, uses OR instead of AND for multiple search terms,
   enabling bag-of-words style matching."
  (if (null search-terms)
      (mapcar #'car (sqlite:execute-to-list (db-conn self)
                                            (format nil "SELECT content FROM cache LIMIT ~D" limit)))
      (let ((query "SELECT content FROM cache WHERE ")
            (connector (if match-any " OR " " AND "))
            (params '()))
        (loop for term in search-terms
              for i from 0
              do (setf query (concatenate 'string query (if (> i 0) connector "") "content LIKE ?"))
                 (push (format nil "%~A%" term) params))
        (setf query (concatenate 'string query (format nil " LIMIT ~D" limit)))
        (mapcar #'car (apply #'sqlite:execute-to-list (db-conn self) query (nreverse params))))))

(defmethod count-items ((self cache-engine))
  "Returns the number of items in the cache."
  (sqlite:execute-single (db-conn self) "SELECT COUNT(*) FROM cache"))

(defmethod clear-cache ((self cache-engine))
  "Removes all items from the cache."
  (sqlite:execute-non-query (db-conn self) "DELETE FROM cache"))

(defmethod clear-cache-older-one-week ((self cache-engine))
  "Removes items older than 7 days from the cache."
  (sqlite:execute-non-query
   (db-conn self)
   "DELETE FROM cache WHERE created_at <= datetime('now', '-7 days')"))

(defmethod close-cache ((self cache-engine))
  "Closes the SQLite database connection."
  (when (slot-boundp self 'db-conn)
    (sqlite:disconnect (db-conn self))))
