(defun read-from-string-test ()
  "read a maximum of 1000 expressions from a string"
  (let ((str "1 2 \"My parrot is named Brady.\" (11 22)"))
    (with-input-from-string
     (input-stream str)
     (dotimes (i 1000)
       (let ((x (read input-stream nil nil)))
	 (if (null x) (return)) ;; break out of the 'dotimes' loop
	 (format t "next expression in string: ~S~%" x))))))

