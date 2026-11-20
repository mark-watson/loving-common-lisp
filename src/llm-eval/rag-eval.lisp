;;;; rag-eval.lisp -- evaluate the agentic RAG example on local models.
;;;;
;;;;   (ql:quickload :llm-eval/rag)
;;;;   (llm-eval::run-rag-eval "baseline")
;;;;
;;;; Needs Ollama with: nomic-embed-text, llama3.2:3b, and qwen3.5:4b.

(in-package #:llm-eval)

(defparameter *rag-cases-file*
  (asdf:system-relative-pathname :llm-eval "data/rag-golden.lisp"))

(defparameter *store-file*
  (asdf:system-relative-pathname :llm-eval "eval-runs.db"))

(defun build-rag-corpus ()
  (setf rag:*embedding-model* "ollama/nomic-embed-text"
        rag:*rag-verbose* nil)
  (let ((corpus (rag:make-corpus :name "sample-docs"
                                 :description "Energy, EVs, and climate")))
    (dolist (file '("renewable-energy.txt" "electric-vehicles.txt" "climate-science.txt"))
      (rag:add-document corpus (asdf:system-relative-pathname :rag (format nil "data/~A" file))))
    corpus))

(defun make-rag-system (corpus &key (model "ollama/llama3.2:3b") (top-k 3) (max-iterations 3))
  "Return a function of one question that answers it with agentic RAG."
  (lambda (question)
    (rag:query corpus question :model model :top-k top-k :max-iterations max-iterations)))

(defun run-rag-eval (label &key (model "ollama/llama3.2:3b") (top-k 3) (max-iterations 3)
                                (corpus (build-rag-corpus)))
  "Run the RAG golden dataset with the given settings, print a report, and
store the run. Returns the run id."
  (let* ((cases (load-cases *rag-cases-file*))
         (results (run-eval (make-rag-system corpus :model model :top-k top-k
                                                    :max-iterations max-iterations)
                            cases))
         (db (open-store (namestring *store-file*))))
    (unwind-protect
         (progn
           (print-report results)
           (save-run db label
                     (list :model model :top-k top-k :max-iterations max-iterations
                           :judge *judge-model*)
                     results))
      (sqlite:disconnect db))))
