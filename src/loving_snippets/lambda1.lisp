(defun test ()
  (let ((my-func
	 (lambda (x) (+ x 1))))
    (funcall my-func 1)))

(defun testfn (a-function a-value)
  (a-function a-value))