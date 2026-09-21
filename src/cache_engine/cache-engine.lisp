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

(defun escape-like-pattern (string)
  "Escape the LIKE metacharacters in STRING so that it matches literally.

The backslash must be escaped first, then % and _. The query must say
ESCAPE '\\', otherwise SQLite treats the backslash as an ordinary
character and % and _ keep their wildcard meaning."
  (with-output-to-string (out)
    (loop for ch across string
          do (when (member ch '(#\\ #\% #\_))
               (write-char #\\ out))
             (write-char ch out))))

(defmethod add_cache ((self cache-engine) text)
  "Adds a string to the cache.

Identical entries are never duplicated: when TEXT is already present its
timestamp is refreshed instead, which keeps a re-cached answer from being
swept up by CLEAR-CACHE-OLDER-ONE-WEEK.

Returns :INSERTED when a new row was written, or :REFRESHED when an
existing row was touched."
  (check-type text string)
  (let ((existing (sqlite:execute-single
                   (db-conn self)
                   "SELECT id FROM cache WHERE content = ? LIMIT 1"
                   text)))
    (if existing
        (progn
          (sqlite:execute-non-query
           (db-conn self)
           "UPDATE cache SET created_at = CURRENT_TIMESTAMP WHERE id = ?"
           existing)
          :refreshed)
        (progn
          (sqlite:execute-non-query
           (db-conn self)
           "INSERT INTO cache (content) VALUES (?)"
           text)
          :inserted))))

(defmethod lookup ((self cache-engine) search-terms &key (limit 3) match-any)
  "Returns matching cached strings (default limit 3).

When MATCH-ANY is T, uses OR instead of AND for multiple search terms,
enabling bag-of-words style matching.

Each term is matched as a literal substring: the LIKE metacharacters % and _
are escaped, so a search term containing them cannot turn into a wildcard
that matches unrelated entries. Matching is case-insensitive for ASCII,
which is why callers usually down-case their terms first."
  (if (null search-terms)
      (mapcar #'car (sqlite:execute-to-list (db-conn self)
                                            (format nil "SELECT content FROM cache LIMIT ~D" limit)))
      (let ((query "SELECT content FROM cache WHERE ")
            (connector (if match-any " OR " " AND "))
            (params '()))
        (loop for term in search-terms
              for i from 0
              do (check-type term string)
                 (setf query (concatenate 'string query
                                          (if (> i 0) connector "")
                                          "content LIKE ? ESCAPE '\\'"))
                 (push (format nil "%~A%" (escape-like-pattern term)) params))
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
  "Closes the SQLite database connection. Safe to call more than once.

After a successful close the DB-CONN slot is unbound, so a second call is a
no-op rather than an error — SQLITE:DISCONNECT leaves the connection object
in a state where touching it signals UNBOUND-SLOT."
  (when (slot-boundp self 'db-conn)
    (sqlite:disconnect (db-conn self))
    (slot-makunbound self 'db-conn)))
