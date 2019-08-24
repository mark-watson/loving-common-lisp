(defun readline-test ()
  "read a maximum of 1000 expresions from the file 'test.dat'"
  (with-open-file
   (input-stream "test.dat" :direction :input)
   (dotimes (i 1000)
     (let ((x (read-line input-stream nil nil)))
       (if (null x) (return)) ;; break out of the 'dotimes' loop
       (format t "next line in file: ~S~%" x)))))
