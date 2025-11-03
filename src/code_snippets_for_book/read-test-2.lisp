(let ((a-path-name
          (make-pathname :directory "testdata"
                         :name "test.dat")))
    (with-open-file
     (input-stream a-path-name :direction :input)))
