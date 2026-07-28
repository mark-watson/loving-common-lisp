;;;; unify.lisp --- Logic variables, neural predicates, and unification.

(in-package :nsk)

;;; Logic variables are interned by name so that two occurrences of the same
;;; name share one object. This lets ASSOC use structure equality safely and
;;; keeps printed output readable.

(defvar *logic-vars* (make-hash-table :test 'eq)
  "Interns logic variables by name.")

(defstruct (logic-var (:constructor %make-logic-var (name))
                      (:predicate logic-var-p)
                      (:copier nil))
  (name nil :read-only t))

(defun logic-var (name)
  "Return the canonical logic variable named NAME (a symbol)."
  (or (gethash name *logic-vars*)
      (setf (gethash name *logic-vars*) (%make-logic-var name))))

(defmethod print-object ((v logic-var) stream)
  (format stream "?~a" (logic-var-name v)))

;;; A neural predicate marks a relation that should fall back to the language
;;; model when no exact symbolic match exists.

(defstruct (neural-predicate (:constructor %make-neural-predicate (name))
                             (:predicate neural-predicate-p)
                             (:copier nil))
  (name nil :read-only t))

(defun neural-predicate (name)
  "Wrap NAME as a neural (LLM fallback) predicate."
  (%make-neural-predicate name))

(defmethod print-object ((p neural-predicate) stream)
  (format stream "~~~a" (neural-predicate-name p)))

;;; Unification, in the Norvig/PAIP style. The environment is an alist of
;;; (logic-var . value). +FAIL+ is a distinct sentinel so that NIL can serve
;;; as the empty (successful) environment.

(defconstant +fail+ 'fail "Sentinel returned by UNIFY on failure.")

(defun unify (x y &optional (env nil))
  "Unify X and Y under ENV, returning an extended environment or +FAIL+."
  (cond ((eq env +fail+) +fail+)
        ((eql x y) env)
        ((logic-var-p x) (unify-var x y env))
        ((logic-var-p y) (unify-var y x env))
        ((and (consp x) (consp y))
         (unify (cdr x) (cdr y)
                (unify (car x) (car y) env)))
        ((and (stringp x) (stringp y) (string= x y)) env)
        (t +fail+)))

(defun unify-var (var x env)
  "Unify logic variable VAR against X, following any existing binding."
  (let ((binding (assoc var env :test #'equalp)))
    (if binding
        (unify (cdr binding) x env)
        (acons var x env))))

(defun resolve (x env)
  "Replace bound variables in X with their values from ENV, recursively."
  (cond ((logic-var-p x)
         (let ((b (assoc x env :test #'equalp)))
           (if b (resolve (cdr b) env) x)))
        ((consp x) (cons (resolve (car x) env) (resolve (cdr x) env)))
        (t x)))
