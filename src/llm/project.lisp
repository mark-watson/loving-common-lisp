(require "asdf")

(let ((asd-path (make-pathname :name "llm" :type "asd" :defaults *load-pathname*)))
  (asdf:load-asd asd-path))

(ql:quickload :llm)

(format t "~%--- llm project loaded ---~%")
