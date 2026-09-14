(require "asdf")

;; The rag system has no LLM library dependency: its HTTP helper and the
;; Gemini generate call are implemented inside the system itself.

;; Load the rag system
(let ((asd-path (make-pathname :name "rag" :type "asd" :defaults *load-pathname*)))
  (asdf:load-asd asd-path))

(ql:quickload :rag)

(format t "~%--- rag project loaded ---~%")
