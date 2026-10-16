(let ((a-path-name
          ;; a bare string :directory means (:absolute "testdata"), i.e.
          ;; /testdata/; use a :relative list for the testdata subdirectory:
          (make-pathname :directory '(:relative "testdata")
                         :name "test" :type "dat")))
    (with-open-file
     (input-stream a-path-name :direction :input)))
