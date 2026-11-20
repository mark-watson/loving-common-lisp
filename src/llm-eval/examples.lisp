;;;; examples.lisp -- evaluate the RAG example three times and compare runs.
;;;;
;;;;   (ql:quickload :llm-eval/rag)
;;;;   (load "examples.lisp")

(in-package #:llm-eval)

(let* ((corpus (build-rag-corpus))
       (baseline (run-rag-eval "baseline" :corpus corpus))
       (baseline-again (run-rag-eval "baseline again" :corpus corpus))
       (less-context (run-rag-eval "top-k 1, 1 iteration" :corpus corpus
                                                          :top-k 1 :max-iterations 1))
       (db (open-store (namestring *store-file*))))
  (unwind-protect
       (progn
         (terpri)
         (print-run-summaries db)
         (terpri)
         (print-comparison db baseline baseline-again)
         (terpri)
         (print-comparison db baseline less-context))
    (sqlite:disconnect db)))
