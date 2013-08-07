;; first simple macro example:

(defmacro double-list (a-list)
  (let ((ret (gensym)))
    `(let ((,ret nil))
       (dolist (x ,a-list)
         (setq ,ret (append ,ret (list x x))))
       ,ret)))

;; use the macro:

(defun test (x)
  (double-list x))

