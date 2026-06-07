(require "asdf")

;; Load the llm dependency from src/llm (sibling directory)
(let* ((rag-dir (make-pathname :name nil :type nil :defaults *load-pathname*))
       (src-dir (merge-pathnames "../" rag-dir))
       (llm-asd (merge-pathnames "llm/llm.asd" src-dir)))
  (asdf:load-asd llm-asd))

(ql:quickload :llm)

;; Now load the rag system
(let ((asd-path (make-pathname :name "rag" :type "asd" :defaults *load-pathname*)))
  (asdf:load-asd asd-path))

(ql:quickload :rag)

(format t "~%--- rag project loaded ---~%")
