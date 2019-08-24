;; a simple loop using recursion

(defun recursion1 (value)
  (format t "entering recursion1(~A)~%" value)
  (if (< value 5)
      (recursion1 (1+ value))))
