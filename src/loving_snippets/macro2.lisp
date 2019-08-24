;; another macro example that uses ,@:

(defmacro double-args (&rest args)
  `(let ((ret nil))
    (dolist (x ,@args)
      (setq ret (append ret (list x x))))
    ret))

;; use the macro:

(defun test (&rest x)
  (double-args x))
