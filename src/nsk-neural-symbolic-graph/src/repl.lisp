;;;; repl.lisp --- The interactive NSK read-eval-print loop.
;;;;
;;;; The loop reads Common Lisp with the NSK readtable active, so triple and
;;;; variable syntax works alongside normal evaluation. Keyword forms act as
;;;; shell commands (:help, :add, :facts, and so on).

(in-package :nsk)

(defparameter *banner*
  "NSK: Neural-Symbolic Knowledge Graph Engine
Type :help for commands, :quit to exit.")

(defparameter *bare-commands* '(:help :quit :exit :facts :count :save)
  "Commands typed as a single keyword.")

(defparameter *list-commands* '(:add :del :ingest)
  "Commands typed as a list whose head is a keyword.")

(defun repl-command-p (form)
  (or (and (keywordp form) (member form *bare-commands*))
      (and (consp form) (keywordp (car form)) (member (car form) *list-commands*))))

(defun print-help ()
  ;; WRITE-STRING, not FORMAT: the help text shows ~ syntax literally, and FORMAT
  ;; would read those tildes as directives.
  (fresh-line)
  (write-string "Commands:
  :help                 show this help
  :facts                list every triple
  :count                show the triple count
  :add s p o            add a triple, e.g. (:add :mark :wrote :nsk)
  :del s p o            remove a triple
  :ingest \"text\"        extract triples from text with the model
  :save                 flush the log to disk
  :quit                 leave the REPL

Queries use the NSK syntax:
  [?who :wrote :nsk]                       one pattern
  (ask (?a) [?a :wrote :nsk] [?a :codes-in :lisp])
  [:mark ~:codes-in ?lang]                 ~ falls back to the model
"))

(defun run-repl-command (form)
  "Run a command FORM. Return :QUIT to leave the loop, otherwise NIL."
  (let ((cmd (if (consp form) (car form) form))
        (args (if (consp form) (cdr form) nil)))
    (case cmd
      ((:quit :exit) :quit)
      (:help (print-help) nil)
      (:count (format t "~&~d triples~%" (triple-count)) nil)
      (:save (when *graph* (finish-output (graph-log-stream *graph*)))
             (format t "~&saved.~%") nil)
      (:facts
       (dolist (tr (all-triples)) (format t "~&  ~{~a~^  ~}~%" tr))
       (format t "~&(~d triples)~%" (triple-count)) nil)
      (:add (destructuring-bind (s p o) args
              (add-triple s p o)
              (format t "~&added ~a ~a ~a~%" s p o)) nil)
      (:del (destructuring-bind (s p o) args
              (remove-triple s p o)
              (format t "~&removed ~a ~a ~a~%" s p o)) nil)
      (:ingest (destructuring-bind (text) args
                 (let ((added (ingest-text text)))
                   (format t "~&ingested ~d triple~:p~%" (length added)))) nil)
      (t (format t "~&unknown command: ~a~%" cmd) nil))))

(defun repl-print (value)
  (if (query-result-p value)
      (format t "~&~a~%" value)
      (format t "~&=> ~s~%" value)))

(defun repl (&optional (graph *graph*))
  "Start the interactive loop against GRAPH (or *GRAPH*)."
  (let ((*graph* (or graph *graph* (make-graph)))
        (*readtable* *nsk-readtable*)
        (*package* (find-package :nsk)))
    (format t "~&~a~%" *banner*)
    (loop
      (format t "~&nsk> ")
      (finish-output)
      (let ((form (handler-case (read *standard-input* nil :eof)
                    (end-of-file () :eof)
                    (error (e)
                      (format t "~&; read error: ~a~%" e)
                      (clear-input)
                      :skip))))
        (cond
          ((eq form :eof) (return))
          ((eq form :skip) nil)
          ((repl-command-p form)
           (when (eq :quit (run-repl-command form)) (return)))
          (t (handler-case (repl-print (eval form))
               (error (e) (format t "~&; error: ~a~%" e)))))))
    (format t "~&Bye.~%")))
