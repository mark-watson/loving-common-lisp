(require :asdf)
(push (truename "./") asdf:*central-registry*)

;; Mocking sqlite if not available, but ideally it should be installed.
;; Let's try to load it.
(handler-case
    (ql:quickload :cache-engine)
  (error (c)
    (format t "Error loading cache-engine: ~A~%" c)
    (uiop:quit 1)))

(use-package :cache-engine)

(when (probe-file "test.db")
  (delete-file "test.db"))

(let ((cache (make-instance 'cache-engine :db-path "test.db")))
  (unwind-protect
       (progn
         (add_cache cache "The quick brown fox jumps over the lazy dog")
         (add_cache cache "Hello world")
         (add_cache cache "Common Lisp is powerful")
         (add_cache cache "SQLite is a great database")
         (add_cache cache "Lisp is the best")
         
         ;; Manually insert an old record to test cleanup
         (sqlite:execute-non-query (cache-engine::db-conn cache)
                                   "INSERT INTO cache (content, created_at) VALUES (?, datetime('now', '-8 days'))"
                                   "Very old item")

         (format t "Cache count before cleanup: ~D~%" (count-items cache))
         (assert (= (count-items cache) 6))

         (clear-cache-older-one-week cache)
         (format t "Cache count after cleanup: ~D~%" (count-items cache))
         (assert (= (count-items cache) 5))

         (format t "Lookup 'fox': ~A~%" (lookup cache '("fox")))
         (format t "Lookup 'Lisp': ~A~%" (lookup cache '("Lisp")))
         (format t "Lookup 'great': ~A~%" (lookup cache '("great")))
         (format t "Lookup 'nothing': nil~%")
         (format t "Lookup 'the': ~A~%" (lookup cache '("the")))
         
         (assert (equal (lookup cache '("fox")) '("The quick brown fox jumps over the lazy dog")))
         (assert (equal (length (lookup cache '("the"))) 2))

         ;; LIKE metacharacters must match literally, not as wildcards
         (add_cache cache "Battery is at 50% charge")
         (add_cache cache "50 percent off deals")
         (format t "Lookup '50%': ~A~%" (lookup cache '("50%")))
         (assert (equal (lookup cache '("50%")) '("Battery is at 50% charge"))
                 () "50% must not act as a LIKE wildcard")
         (add_cache cache "file_name convention")
         (add_cache cache "fileXname convention")
         (assert (equal (lookup cache '("file_name")) '("file_name convention"))
                 () "_ must not act as a single-character LIKE wildcard")

         ;; Duplicates are refreshed, not inserted twice
         (assert (eq (add_cache cache "Hello world") :refreshed))
         (assert (eq (add_cache cache "A brand new entry") :inserted))
         (assert (= 1 (length (lookup cache '("Hello world") :limit 10))))
         (format t "Duplicate add returned :refreshed, row count unchanged.~%")

         ;; Refreshing keeps an entry out of the one-week sweep
         (sqlite:execute-non-query (cache-engine::db-conn cache)
                                   "INSERT INTO cache (content, created_at) VALUES (?, datetime('now', '-8 days'))"
                                   "stale answer")
         (add_cache cache "stale answer")
         (clear-cache-older-one-week cache)
         (assert (plusp (length (lookup cache '("stale answer") :limit 5)))
                 () "a refreshed entry must survive the one-week sweep")

         (clear-cache cache)
         (format t "After clear, count: ~D~%" (count-items cache))
         (assert (= (count-items cache) 0))

         ;; close-cache is idempotent: this call and the one in the cleanup
         ;; below must both be safe
         (close-cache cache)
         (close-cache cache)
         (format t "close-cache is idempotent.~%"))
    (close-cache cache)
    (when (probe-file "test.db")
      (delete-file "test.db"))))

(format t "Tests passed!~%")
(uiop:quit 0)
