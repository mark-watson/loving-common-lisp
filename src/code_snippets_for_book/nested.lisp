(flet ((add-one (x)
         (+ x 1))
       (add-two (x)
         (+ x 2)))
  (format t "redefined variables: ~A  ~A~%" (add-one 100) (add-two 100)))

(let ((a 3.14))
  (defun test2 (x) ; this works, but don't do it!
    (print x))
  (test2 a))

(test2 50)

(let ((x 1)
      (y 2))
  ;; properly define a test function nested inside a let statement:
  (flet ((test (a b)
           (let ((z (+ a b)))
             ;; define a helper function nested inside a let/function/let:
             (flet ((nested-function (a)
                      (+ a a)))
               (nested-function z)))))
    ;; call nested function 'test':
    (format t "test result is ~A~%" (test x y))))

(let ((z 10))
  (labels ((test-recursion (a)
             (format t "test-recursion ~A~%" (+ a z))
             (if (> a 0)
                 (test-recursion (- a 1)))))
    (test-recursion 5)))
