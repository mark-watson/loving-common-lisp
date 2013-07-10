;; example do macro use

(do ((i 0 (1+ i)))
    ((> i 3) "value-of-do-loop")
  (print i))
