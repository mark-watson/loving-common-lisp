;;;; store.lisp --- The triplestore: in-memory indices plus a disk log.
;;;;
;;;; Triples are (subject predicate object). Two hash tables index triples by
;;;; subject and by object for fast pattern narrowing. State survives restarts
;;;; through an append-only log of s-expressions that is replayed on startup.

(in-package :nsk)

(defparameter *log-path* #p"nsk-graph.log"
  "Default path of the append-only transaction log.")

(defstruct (graph (:constructor %make-graph) (:copier nil))
  (spo (make-hash-table :test 'equalp))   ; subject -> list of triples
  (osp (make-hash-table :test 'equalp))   ; object  -> list of triples
  (triples '())                            ; every live triple, newest first
  (count 0)
  (log-stream nil)
  (log-path nil))

(defvar *graph* nil "The active knowledge graph.")

(defun make-graph ()
  "Create an empty in-memory graph with no backing log."
  (%make-graph))

;;; Indexing primitives (no logging)

(defun %index (graph triple)
  (destructuring-bind (s p o) triple
    (declare (ignore p))
    (push triple (gethash s (graph-spo graph)))
    (push triple (gethash o (graph-osp graph)))
    (push triple (graph-triples graph))
    (incf (graph-count graph)))
  triple)

(defun %unindex (graph triple)
  (destructuring-bind (s p o) triple
    (declare (ignore p))
    (setf (gethash s (graph-spo graph))
          (remove triple (gethash s (graph-spo graph)) :test #'equalp))
    (setf (gethash o (graph-osp graph))
          (remove triple (gethash o (graph-osp graph)) :test #'equalp))
    (setf (graph-triples graph)
          (remove triple (graph-triples graph) :test #'equalp))
    (decf (graph-count graph)))
  triple)

(defun triple-present-p (graph triple)
  (member triple (gethash (first triple) (graph-spo graph)) :test #'equalp))

;;; Logging

(defun %log (graph entry)
  (let ((s (graph-log-stream graph)))
    (when s
      (let ((*package* (find-package :nsk))
            (*print-readably* nil)
            (*print-pretty* nil))
        (prin1 entry s)
        (terpri s)
        (finish-output s)))))

;;; Public mutation API

(defun add-triple (s p o &optional (graph *graph*))
  "Add triple (S P O) to GRAPH and append it to the log. Duplicates are ignored."
  (let ((triple (list s p o)))
    (unless (triple-present-p graph triple)
      (%index graph triple)
      (%log graph (list :add s p o)))
    triple))

(defun remove-triple (s p o &optional (graph *graph*))
  "Remove triple (S P O) from GRAPH and record the deletion in the log."
  (let ((triple (list s p o)))
    (when (triple-present-p graph triple)
      (%unindex graph triple)
      (%log graph (list :del s p o)))
    triple))

(defun all-triples (&optional (graph *graph*))
  "Return every live triple in insertion order."
  (reverse (graph-triples graph)))

(defun triple-count (&optional (graph *graph*))
  (graph-count graph))

;;; Index-driven candidate selection

(defun indexable-p (term)
  "True when TERM is a concrete value usable as an index key."
  (not (or (logic-var-p term) (neural-predicate-p term))))

(defun candidate-triples (graph pattern)
  "Return the triples that could match PATTERN, narrowed by the indices."
  (destructuring-bind (s p o) pattern
    (declare (ignore p))
    (cond ((indexable-p s) (gethash s (graph-spo graph)))
          ((indexable-p o) (gethash o (graph-osp graph)))
          (t (graph-triples graph)))))

;;; Persistence: replay, open, close

(defun apply-log-entry (graph entry)
  (destructuring-bind (op s p o) entry
    (ecase op
      (:add (let ((tr (list s p o)))
              (unless (triple-present-p graph tr) (%index graph tr))))
      (:del (let ((tr (list s p o)))
              (when (triple-present-p graph tr) (%unindex graph tr)))))))

(defun replay-log (graph path)
  "Rebuild GRAPH by replaying the log at PATH in order."
  (with-open-file (in path :direction :input :if-does-not-exist nil
                           :external-format :utf-8)
    (when in
      ;; *read-eval* is disabled so a stray #. in the log cannot run code.
      (let ((*read-eval* nil)
            (*package* (find-package :nsk)))
        (loop for entry = (read in nil :eof)
              until (eq entry :eof)
              do (apply-log-entry graph entry))))))

(defun open-store (&optional (path *log-path*))
  "Open (or create) the store at PATH, replay its log, and keep it open for
   appending. Returns the graph."
  (let ((graph (%make-graph :log-path path)))
    (when (probe-file path)
      (replay-log graph path))
    (setf (graph-log-stream graph)
          (open path :direction :output :if-exists :append
                     :if-does-not-exist :create :external-format :utf-8))
    graph))

(defun close-store (&optional (graph *graph*))
  "Flush and close the log stream backing GRAPH."
  (when (and graph (graph-log-stream graph))
    (finish-output (graph-log-stream graph))
    (close (graph-log-stream graph))
    (setf (graph-log-stream graph) nil))
  graph)
