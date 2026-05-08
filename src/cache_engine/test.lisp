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

         (clear-cache cache)
         (format t "After clear, count: ~D~%" (count-items cache))
         (assert (= (count-items cache) 0)))
    (close-cache cache)
    (when (probe-file "test.db")
      (delete-file "test.db"))))

(format t "Tests passed!~%")
(uiop:quit 0)
