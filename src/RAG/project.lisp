(require "asdf")

;; All LLM access goes through the litelm routing library, which lives in the
;; sibling src/litelm directory. Register it first so ASDF can resolve the
;; :rag system's litelm dependency when this project is loaded on its own.

(let ((litelm-asd (merge-pathnames
                   (make-pathname :directory '(:relative :up "litelm")
                                  :name "litelm" :type "asd")
                   *load-pathname*)))
  (when (probe-file litelm-asd)
    (asdf:load-asd litelm-asd)))

;; Load the rag system
(let ((asd-path (make-pathname :name "rag" :type "asd" :defaults *load-pathname*)))
  (asdf:load-asd asd-path))

(ql:quickload :rag)

(format t "~%--- rag project loaded ---~%")
