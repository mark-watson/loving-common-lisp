(defun read-test-2 ()
  "using pathname, read a maximum of 1000 expresions from the file 'test.dat'"
  (let ((a-path-name (make-pathname :directory "testdata" :name "test.dat")))
    (with-open-file
     (input-stream a-path-name :direction :input)
     (dotimes (i 1000)
       (let ((x (read input-stream nil nil)))
	 (if (null x) (return)) ;; break out of the 'dotimes' loop
	 (format t "next expression in file: ~S~%" x))))))

