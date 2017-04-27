(add-to-classpath '("Simple.jar"))

(defun add2 (i1 i2)
  (print (jclass "Simple"))
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "Simple"))
         (intclass (jclass "int"))
         (method (jmethod class "addTwoNumbers" intclass intclass))
         (result (jcall method param i1 i2)))
    (format t "in add2, result of calling addTwoNumbers(2, 4): ~a~%" result)
    result))

(defun modify-string (s)
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "Simple"))
         (string-class (jclass "java.lang.String"))
         (method (jmethod class "modifyString" string-class))
         (result (jcall method param s)))
    (format t "in add2, result of calling modify-string(...)): ~a~%" result)
    result))

(defun modify-list (ls)
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "Simple"))
         (string-class (jclass "java.lang.Object"))
         (method (jmethod class "modifyArray" string-class))
         (result (jcall method param (jarray-from-list ls))))
    (format t "in mls, result of calling modifyArray(...): ~a~%" result)
    result))
