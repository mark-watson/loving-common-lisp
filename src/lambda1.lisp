(defun test ()
  (let ((my-func
	 (lambda (x) (+ x 1))))
    (funcall my-func 1)))