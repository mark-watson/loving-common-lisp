(let ((x 1)
      (y 2))
  ;; define a test function nested inside a let statement:
  (defun test (a b)
    (let ((z (+ a b)))
      ;; define a helper function nested inside a let/function/let:
      (defun nested-function (a)
        (+ a a))
      (nested-function z)))
  ;; print a few blank lines, then test function 'test':
  (format t "~%~%test result is ~A~%~%" (test x y)))

