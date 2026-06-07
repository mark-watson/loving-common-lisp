;;; rag.lisp — Top-level API, interactive demo, and test code
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(in-package #:rag)

;;; ---- Public API ----

(defun query (corpora question)
  "Ask QUESTION against the given CORPORA using agentic RAG.
   CORPORA is a list of corpus structs (or a single corpus).
   Returns the answer string."
  (let ((corpus-list (if (listp corpora) corpora (list corpora))))
    (agentic-rag corpus-list question)))

;;; ---- Interactive Demo ----

(defun interactive-demo (corpora)
  "Start an interactive REPL for querying CORPORA.
   Type 'quit' or 'exit' to stop."
  (let ((corpus-list (if (listp corpora) corpora (list corpora))))
    (format t "~%~%============================~%")
    (format t "  Agentic RAG Interactive Demo~%")
    (format t "============================~%")
    (format t "~%Loaded ~A corpora with ~A total chunks.~%"
            (length corpus-list)
            (loop for c in corpus-list sum (length (corpus-chunks c))))
    (format t "Type your question (or 'quit' to exit):~%")
    (loop
      (format t "~%RAG> ")
      (finish-output)
      (let ((input (read-line *standard-input* nil)))
        (when (or (null input)
                  (member input '("quit" "exit" "q") :test #'string-equal))
          (format t "~%Goodbye!~%")
          (return))
        (when (> (length (string-trim '(#\Space) input)) 0)
          (let ((answer (agentic-rag corpus-list input)))
            (format t "~%~%===== ANSWER =====~%~A~%==================~%"
                    answer)))))))

;;; ---- Test / Demo ----

(defvar *base-pathname*
  (asdf:system-source-directory :rag)
  "Base directory of the RAG system source, used to locate data/ files.")

(defun data-path (filename)
  "Resolve a filename relative to the data/ subdirectory."
  (merge-pathnames (concatenate 'string "data/" filename)
                   *base-pathname*))

(defun test ()
  "Run a demo of the Agentic RAG system with sample documents.
   Creates three corpora (energy, vehicles, climate) and runs
   multi-hop queries that require cross-corpus retrieval."
  (format t "~%~%============================================~%")
  (format t "  Agentic RAG Demo — Loading Documents~%")
  (format t "============================================~%")

  ;; Create three separate corpora to demonstrate cross-corpus retrieval
  (let ((energy-corpus (make-corpus :name "renewable-energy"
                                    :description "Renewable energy sources and technologies"))
        (ev-corpus (make-corpus :name "electric-vehicles"
                                :description "Electric vehicle technology and infrastructure"))
        (climate-corpus (make-corpus :name "climate-science"
                                    :description "Climate science and carbon emissions")))
    
    ;; Load documents into their respective corpora
    (add-document energy-corpus (data-path "renewable-energy.txt"))
    (add-document ev-corpus (data-path "electric-vehicles.txt"))
    (add-document climate-corpus (data-path "climate-science.txt"))
    
    (let ((all-corpora (list energy-corpus ev-corpus climate-corpus)))
      (format t "~%~%Loaded ~A total chunks across ~A corpora.~%"
              (loop for c in all-corpora sum (length (corpus-chunks c)))
              (length all-corpora))
      
      ;; Query 1: Single-corpus question (should find answer easily)
      (format t "~%~%===== TEST QUERY 1 (single topic) =====~%")
      (let ((answer (query all-corpora
                           "What is the current cost of lithium-ion battery storage per kilowatt-hour?")))
        (format t "~%~%ANSWER 1:~%~A~%~%" answer))
      
      ;; Query 2: Multi-hop question requiring cross-corpus retrieval
      (format t "~%~%===== TEST QUERY 2 (multi-hop, cross-corpus) =====~%")
      (let ((answer (query all-corpora
                           "How does the carbon footprint of manufacturing EV batteries compare to the emissions saved by charging EVs from renewable energy sources?")))
        (format t "~%~%ANSWER 2:~%~A~%~%" answer))
      
      ;; Query 3: Complex question that may need iterative retrieval
      (format t "~%~%===== TEST QUERY 3 (complex, iterative) =====~%")
      (let ((answer (query all-corpora
                           "What role could solid-state batteries and pumped-storage hydroelectricity play together in solving the intermittency problem of wind and solar energy?")))
        (format t "~%~%ANSWER 3:~%~A~%~%" answer))
      
      (format t "~%~%============================================~%")
      (format t "  Demo Complete~%")
      (format t "============================================~%")
      
      ;; Return corpora for interactive use
      all-corpora)))
