;;;; query.lisp --- Pattern matching, neural fallback, and the ASK macro.
;;;;
;;;; A single triple pattern yields a list of environments (one per match).
;;;; Conjunctions thread environments forward with MAPCAN. When a ~ predicate
;;;; finds no symbolic match, the engine asks the model for the missing object.

(in-package :nsk)

(defun ground-pattern (pattern env)
  "Replace bound variables in PATTERN with their values from ENV."
  (mapcar (lambda (term)
            (if (logic-var-p term)
                (let ((b (assoc term env :test #'equalp)))
                  (if b (cdr b) term))
                term))
          pattern))

(defun match-triple-pattern (pattern env graph)
  "Return every environment that satisfies PATTERN under ENV. A neural
   predicate first tries a plain symbolic match on its bare name, then falls
   back to the model."
  (let* ((pred (second pattern))
         (neuralp (neural-predicate-p pred))
         ;; For symbolic matching, unwrap a neural predicate to its bare name.
         (spat (if neuralp
                   (list (first pattern) (neural-predicate-name pred) (third pattern))
                   pattern))
         (gpat (ground-pattern spat env))
         (results '()))
    (dolist (tr (candidate-triples graph gpat))
      (let ((e (unify spat tr env)))
        (unless (eq e +fail+) (push e results))))
    (when (and (null results) neuralp)
      (let ((e (neural-match gpat spat env)))
        (when (and e (not (eq e +fail+))) (push e results))))
    (nreverse results)))

(defun neural-match (grounded spat env)
  "Resolve a neural predicate by asking the model for the unknown object."
  (destructuring-bind (subject predicate object) grounded
    (declare (ignore object))
    (when (indexable-p subject)                 ; subject must be concrete
      (let ((target (third spat)))
        (when (logic-var-p target)
          (let ((answer (query-neural-fallback subject predicate)))
            (when answer
              (unify target (sanitize-to-keyword answer) env))))))))

(defun prove (patterns env graph)
  "Prove PATTERNS as a conjunction, threading environments forward."
  (if (null patterns)
      (list env)
      (mapcan (lambda (e) (prove (cdr patterns) e graph))
              (match-triple-pattern (car patterns) env graph))))

(defun collect-vars (form &optional acc)
  "Collect the distinct logic variables appearing in FORM."
  (cond ((logic-var-p form)
         (if (member form acc :test #'equalp) acc (cons form acc)))
        ((consp form) (collect-vars (cdr form) (collect-vars (car form) acc)))
        (t acc)))

;;; A query result wraps the raw solutions so the REPL can print a readable
;;; table while callers can still pull the data out with SOLUTIONS.

(defstruct (query-result (:constructor make-query-result (solutions)))
  solutions)

(defmethod print-object ((r query-result) stream)
  (let ((sols (query-result-solutions r)))
    (cond ((null sols) (format stream "#<no solutions>"))
          ((equal sols '(())) (format stream "yes"))
          (t (format stream "~{~a~^~%~}"
                     (mapcar (lambda (sol)
                               (format nil "~{~a=~s~^, ~}"
                                       (loop for (k . v) in sol
                                             append (list (string-downcase (symbol-name k)) v))))
                             sols))))))

(defun solutions (result)
  "Return the raw list of solutions from a query result (or a plain list)."
  (if (query-result-p result) (query-result-solutions result) result))

(defun run-query (patterns result-vars &optional (graph *graph*))
  "Prove PATTERNS and return a QUERY-RESULT binding each of RESULT-VARS."
  (let ((sols (mapcar (lambda (env)
                        (mapcar (lambda (v)
                                  (cons (logic-var-name v) (resolve v env)))
                                result-vars))
                      (prove patterns nil graph))))
    (make-query-result (remove-duplicates sols :test #'equalp :from-end t))))

(defun match-triple (s p o &optional (graph *graph*))
  "Run one triple pattern, returning a QUERY-RESULT for its variables."
  (let* ((pattern (list s p o))
         (vars (reverse (collect-vars pattern))))
    (run-query (list pattern) vars graph)))

(defmacro ask (result-vars &body clauses)
  "Datalog-style query. RESULT-VARS is a list like (?a ?b); each clause is a
   [s p o] triple pattern. Returns a QUERY-RESULT."
  (let ((patterns
          (mapcar (lambda (clause)
                    (unless (and (consp clause) (eq (first clause) 'match-triple))
                      (error "ASK clause is not a [triple] pattern: ~s" clause))
                    (cons 'list (rest clause)))
                  clauses)))
    `(run-query (list ,@patterns) (list ,@result-vars))))
