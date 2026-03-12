;;;; data.lisp — Core data structures for symbolic mathematics
;;;;
;;;; Provides structs and helper functions for:
;;;;   sym-variable   — a symbolic variable (e.g. x, y, t)
;;;;   sym-constant   — a named constant (e.g. pi, e, 42)
;;;;   sym-term       — a single monomial: coefficient * variable ^ exponent
;;;;   sym-polynomial — a polynomial in one variable (ordered list of terms)
;;;;   sym-integral   — a definite or indefinite integral
;;;;
;;;; This file intentionally contains no side-effects beyond defstruct /
;;;; defun definitions so it can be loaded safely as a library.

(defpackage #:symbolic-math
  (:use #:cl)
  (:export
   ;; Variables
   #:sym-variable
   #:make-variable
   #:variable-p
   #:variable-name
   #:variable-domain
   #:variable=
   ;; Constants
   #:sym-constant
   #:make-constant
   #:constant-p
   #:constant-name
   #:constant-value
   #:constant-numeric-value
   ;; Terms (monomials)
   #:sym-term
   #:make-term
   #:term-p
   #:term-coefficient
   #:term-variable
   #:term-exponent
   #:term=
   #:term-negate
   #:term-scale
   #:term->string
   ;; Polynomials
   #:sym-polynomial
   #:make-polynomial
   #:polynomial-p
   #:polynomial-variable
   #:polynomial-terms
   #:polynomial-domain
   #:polynomial-degree
   #:polynomial-leading-term
   #:polynomial-add
   #:polynomial-subtract
   #:polynomial-negate
   #:polynomial-scale
   #:polynomial->string
   #:polynomial-evaluate
   #:polynomial-normalize
   ;; Polynomial convenience constructors
   #:zero-polynomial
   #:constant-polynomial
   #:identity-polynomial
   ;; Integrals
   #:sym-integral
   #:make-integral
   #:integral-p
   #:integral-integrand
   #:integral-variable
   #:integral-lower
   #:integral-upper
   #:integral-definite-p
   #:integral->string
   ;; Smoke test
   #:run-smoke-test))

(in-package #:symbolic-math)


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 1.  SYM-VARIABLE
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (sym-variable (:conc-name variable-))
  "A symbolic variable such as x, y, or t.
DOMAIN may be :real (default), :complex, or :integer."
  (name   nil :type symbol)
  (domain :real :type keyword))

(defun make-variable (name &key (domain :real))
  "Create a symbolic variable with NAME (a symbol) and optional DOMAIN.
  DOMAIN is one of :real (default), :complex, or :integer.

  Example:
    (make-variable 'x)              ; => sym-variable x ∈ ℝ
    (make-variable 't :domain :real)"
  (check-type name symbol)
  (assert (member domain '(:real :complex :integer))
          (domain) "DOMAIN must be :real, :complex, or :integer, got ~s" domain)
  (make-sym-variable :name name :domain domain))

(defun variable-p (obj)
  "Return T if OBJ is a sym-variable."
  (sym-variable-p obj))

(defun variable= (a b)
  "Return T if variables A and B represent the same symbolic variable.
Two variables are equal when their names and domains match."
  (and (sym-variable-p a)
       (sym-variable-p b)
       (eq (variable-name a) (variable-name b))
       (eq (variable-domain a) (variable-domain b))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 2.  SYM-CONSTANT
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (sym-constant (:conc-name constant-))
  "A named mathematical constant.
VALUE may be a Common Lisp number, :pi, or :e (Euler's number)."
  (name  nil :type symbol)
  (value nil))                          ; number | :pi | :e

(defun make-constant (name value)
  "Create a named constant with NAME (symbol) and VALUE.
VALUE may be a real number, :pi, or :e.

  Examples:
    (make-constant 'pi :pi)
    (make-constant 'e  :e)
    (make-constant 'g  9.80665)"
  (check-type name symbol)
  (assert (or (numberp value) (member value '(:pi :e)))
          (value) "VALUE must be a number, :pi, or :e, got ~s" value)
  (make-sym-constant :name name :value value))

(defun constant-p (obj)
  "Return T if OBJ is a sym-constant."
  (sym-constant-p obj))

(defun constant-numeric-value (c)
  "Return the numeric (floating-point) value of constant C.
:pi → pi, :e → e, numbers are returned as-is."
  (check-type c sym-constant)
  (let ((v (constant-value c)))
    (cond
      ((eq v :pi) pi)
      ((eq v :e)  (exp 1.0d0))
      (t          v))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 3.  SYM-TERM  (monomial:  coefficient * variable ^ exponent)
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (sym-term (:conc-name term-))
  "A single monomial: COEFFICIENT * VARIABLE ^ EXPONENT.
EXPONENT is a non-negative integer (0 gives a constant term)."
  (coefficient 0 :type real)
  (variable    nil)                     ; sym-variable
  (exponent    0 :type (integer 0 *)))

(defun make-term (coefficient variable exponent)
  "Create a monomial: COEFFICIENT * VARIABLE ^ EXPONENT.
EXPONENT must be a non-negative integer.

  Examples:
    (make-term 3  x 2)   ; 3x²
    (make-term -1 x 1)   ; -x
    (make-term 5  x 0)   ; constant 5 (variable ignored for evaluation)"
  (check-type coefficient real)
  (check-type variable sym-variable)
  (check-type exponent  (integer 0 *))
  (make-sym-term :coefficient coefficient
                 :variable    variable
                 :exponent    exponent))

(defun term-p (obj)
  "Return T if OBJ is a sym-term."
  (sym-term-p obj))

(defun term= (a b)
  "Return T if terms A and B are structurally equal
(same variable, exponent, and coefficient)."
  (and (sym-term-p a) (sym-term-p b)
       (= (term-coefficient a) (term-coefficient b))
       (variable= (term-variable a) (term-variable b))
       (= (term-exponent a) (term-exponent b))))

(defun term-negate (term)
  "Return a new term equal to -TERM."
  (make-sym-term :coefficient (- (term-coefficient term))
                 :variable    (term-variable term)
                 :exponent    (term-exponent  term)))

(defun term-scale (term scalar)
  "Return a new term equal to SCALAR * TERM."
  (check-type scalar real)
  (make-sym-term :coefficient (* scalar (term-coefficient term))
                 :variable    (term-variable term)
                 :exponent    (term-exponent  term)))

(defun term->string (term)
  "Return a human-readable string representation of TERM.
Example: 3x^2, -x^1, 5 (for exponent 0)."
  (let ((c (term-coefficient term))
        (v (symbol-name (variable-name (term-variable term))))
        (e (term-exponent term)))
    (cond
      ((zerop e)
       (format nil "~a" c))
      ((= e 1)
       (format nil "~a~a" c v))
      (t
       (format nil "~a~a^~a" c v e)))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 4.  SYM-POLYNOMIAL
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (sym-polynomial (:conc-name polynomial-))
  "A polynomial in one VARIABLE represented as an ordered list of TERMS.
Terms are kept in descending order by exponent.
DOMAIN is :real (default) or :complex."
  (variable nil)                       ; sym-variable
  (terms    '() :type list)            ; list of sym-term, descending exponent
  (domain   :real :type keyword))

(defun polynomial-p (obj)
  "Return T if OBJ is a sym-polynomial."
  (sym-polynomial-p obj))

(defun %sort-and-combine-terms (terms)
  "Internal: combine like-exponent terms, drop zero coefficients, sort descending."
  (let ((table (make-hash-table :test #'eql)))
    ;; accumulate coefficients by exponent
    (dolist (term terms)
      (let ((e (term-exponent term)))
        (setf (gethash e table)
              (+ (gethash e table 0)
                 (term-coefficient term)))))
    ;; rebuild term list, drop zeros, sort descending
    (let ((result '()))
      (maphash (lambda (exp coeff)
                 (unless (zerop coeff)
                   (push (cons exp coeff) result)))
               table)
      result)))

(defun make-polynomial (variable terms &key (domain :real))
  "Create a polynomial in VARIABLE from a list of TERMS (sym-term objects).
Like-exponent terms are combined automatically; zero-coefficient terms are dropped.
Result terms are stored in descending exponent order.

  Example:
    (let ((x (make-variable 'x)))
      (make-polynomial x (list (make-term 3 x 2)
                               (make-term -1 x 1)
                               (make-term 5 x 0))))"
  (check-type variable sym-variable)
  (dolist (term terms)
    (check-type term sym-term)
    (assert (variable= variable (term-variable term))
            (term)
            "Term variable ~s does not match polynomial variable ~s"
            (variable-name (term-variable term))
            (variable-name variable)))
  (assert (member domain '(:real :complex))
          (domain) "DOMAIN must be :real or :complex, got ~s" domain)
  (let* ((pairs   (%sort-and-combine-terms terms))
         (sorted  (sort pairs #'> :key #'car))
         (new-terms (mapcar (lambda (pair)
                              (make-sym-term :coefficient (cdr pair)
                                             :variable    variable
                                             :exponent    (car pair)))
                            sorted)))
    (make-sym-polynomial :variable variable
                         :terms    new-terms
                         :domain   domain)))

(defun polynomial-normalize (poly)
  "Return POLY with terms re-sorted and zero-coefficient terms removed.
Useful after mutation or external term construction."
  (make-polynomial (polynomial-variable poly)
                   (polynomial-terms poly)
                   :domain (polynomial-domain poly)))

(defun polynomial-degree (poly)
  "Return the degree (highest exponent) of POLY.
Returns -1 for the zero polynomial (no terms)."
  (check-type poly sym-polynomial)
  (if (null (polynomial-terms poly))
      -1
      (term-exponent (first (polynomial-terms poly)))))

(defun polynomial-leading-term (poly)
  "Return the leading (highest-degree) term of POLY, or NIL for the zero polynomial."
  (check-type poly sym-polynomial)
  (first (polynomial-terms poly)))

;;; ── Arithmetic ──────────────────────────────────────────────────────────

(defun polynomial-add (p q)
  "Return P + Q as a new polynomial.
P and Q must share the same variable."
  (check-type p sym-polynomial)
  (check-type q sym-polynomial)
  (assert (variable= (polynomial-variable p) (polynomial-variable q))
          () "Cannot add polynomials in different variables: ~s and ~s"
          (variable-name (polynomial-variable p))
          (variable-name (polynomial-variable q)))
  (make-polynomial (polynomial-variable p)
                   (append (polynomial-terms p) (polynomial-terms q))
                   :domain (polynomial-domain p)))

(defun polynomial-negate (poly)
  "Return -POLY as a new polynomial."
  (check-type poly sym-polynomial)
  (make-polynomial (polynomial-variable poly)
                   (mapcar #'term-negate (polynomial-terms poly))
                   :domain (polynomial-domain poly)))

(defun polynomial-subtract (p q)
  "Return P - Q as a new polynomial."
  (polynomial-add p (polynomial-negate q)))

(defun polynomial-scale (poly scalar)
  "Return SCALAR * POLY as a new polynomial."
  (check-type poly sym-polynomial)
  (check-type scalar real)
  (make-polynomial (polynomial-variable poly)
                   (mapcar (lambda (term) (term-scale term scalar))
                           (polynomial-terms poly))
                   :domain (polynomial-domain poly)))

;;; ── Evaluation & Display ────────────────────────────────────────────────

(defun polynomial-evaluate (poly value)
  "Evaluate POLY at VALUE (a number) by substituting the variable.
Returns a number.

  Example: (polynomial-evaluate p 2)  ; evaluate p(2)"
  (check-type poly sym-polynomial)
  (check-type value real)
  (reduce #'+
          (polynomial-terms poly)
          :key (lambda (term)
                 (* (term-coefficient term)
                    (expt value (term-exponent term))))
          :initial-value 0))

(defun polynomial->string (poly)
  "Return a human-readable string representation of POLY.
Terms are printed in descending exponent order, separated by ' + '.
The zero polynomial is rendered as '0'.

  Example: '3x^2 + -1x^1 + 5'"
  (check-type poly sym-polynomial)
  (if (null (polynomial-terms poly))
      "0"
      (format nil "~{~a~^ + ~}"
              (mapcar #'term->string (polynomial-terms poly)))))

;;; ── Convenience constructors ─────────────────────────────────────────────

(defun zero-polynomial (variable)
  "Return the zero polynomial in VARIABLE (no terms)."
  (make-sym-polynomial :variable variable :terms '() :domain :real))

(defun constant-polynomial (variable value)
  "Return the constant polynomial VALUE in VARIABLE.
  Example: (constant-polynomial x 7)  ; represents the polynomial 7"
  (make-polynomial variable
                   (list (make-term value variable 0))))

(defun identity-polynomial (variable)
  "Return the polynomial x (i.e., the degree-1 identity) in VARIABLE.
  Example: (identity-polynomial x)  ; represents x"
  (make-polynomial variable
                   (list (make-term 1 variable 1))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 5.  SYM-INTEGRAL
;;;; ─────────────────────────────────────────────────────────────────────────

(defstruct (sym-integral (:conc-name integral-))
  "Represents a definite or indefinite integral.

Fields:
  INTEGRAND — the expression being integrated (a sym-polynomial or any form)
  VARIABLE  — the variable of integration (sym-variable)
  LOWER     — lower bound (a number, sym-constant, or NIL for indefinite)
  UPPER     — upper bound (a number, sym-constant, or NIL for indefinite)"
  (integrand nil)                      ; sym-polynomial | expression
  (variable  nil)                      ; sym-variable
  (lower     nil)                      ; number | sym-constant | nil
  (upper     nil))                     ; number | sym-constant | nil

(defun integral-p (obj)
  "Return T if OBJ is a sym-integral."
  (sym-integral-p obj))

(defun make-integral (integrand variable &key lower upper)
  "Create a symbolic integral.

  INTEGRAND — the expression to integrate (typically a sym-polynomial)
  VARIABLE  — sym-variable: the variable of integration
  LOWER     — optional lower bound (number or sym-constant); NIL ⇒ indefinite
  UPPER     — optional upper bound (number or sym-constant); NIL ⇒ indefinite

  Examples:
    ;; Indefinite: ∫ (3x² - x + 5) dx
    (make-integral poly x)

    ;; Definite: ∫₀¹ (3x² - x + 5) dx
    (make-integral poly x :lower 0 :upper 1)"
  (check-type variable sym-variable)
  (assert (or (and (null lower) (null upper))
              (and lower upper))
          () "Either both LOWER and UPPER must be provided, or neither.")
  (make-sym-integral :integrand integrand
                     :variable  variable
                     :lower     lower
                     :upper     upper))

(defun integral-definite-p (integral)
  "Return T if INTEGRAL is a definite integral (has bounds)."
  (check-type integral sym-integral)
  (not (null (integral-lower integral))))

(defun %bound->string (bound)
  "Internal: render a bound (number or sym-constant) as a string."
  (cond
    ((null bound) "")
    ((numberp bound) (format nil "~a" bound))
    ((sym-constant-p bound) (symbol-name (constant-name bound)))
    (t (format nil "~a" bound))))

(defun integral->string (integral)
  "Return a human-readable string for INTEGRAL.

Indefinite:  ∫(expr) dx
Definite:    ∫[a,b](expr) dx"
  (check-type integral sym-integral)
  (let* ((var (symbol-name (variable-name (integral-variable integral))))
         (body (cond
                 ((sym-polynomial-p (integral-integrand integral))
                  (polynomial->string (integral-integrand integral)))
                 (t
                  (format nil "~a" (integral-integrand integral)))))
         (bounds (if (integral-definite-p integral)
                     (format nil "[~a,~a]"
                             (%bound->string (integral-lower integral))
                             (%bound->string (integral-upper integral)))
                     "")))
    (format nil "∫~a(~a) d~a" bounds body var)))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 6.  Quick smoke-test (only runs when file loaded as a script)
;;;; ─────────────────────────────────────────────────────────────────────────

(defun run-smoke-test ()
  "Run a quick sanity check printing results to *standard-output*."
  (let* ((x  (make-variable 'x))
         (pi-const (make-constant 'pi :pi))
         ;; Build  3x² - x + 5
         (p  (make-polynomial x
                              (list (make-term  3 x 2)
                                    (make-term -1 x 1)
                                    (make-term  5 x 0))))
         ;; Build  x + 2
         (q  (make-polynomial x
                              (list (make-term 1 x 1)
                                    (make-term 2 x 0))))
         ;; p + q  =>  3x² + 7
         (sum  (polynomial-add p q))
         ;; p - q  =>  3x² - 2x + 3
         (diff (polynomial-subtract p q))
         ;; 2p     =>  6x² - 2x + 10
         (scaled (polynomial-scale p 2))
         ;; Indefinite integral of p
         (indef (make-integral p x))
         ;; Definite integral ∫₀¹ p dx
         (def   (make-integral p x :lower 0 :upper 1))
         ;; Definite integral ∫₀^π p dx  (π as sym-constant bound)
         (pi-int (make-integral p x :lower 0 :upper pi-const)))

    (format t "~%=== Symbolic Math Data Layer Smoke Test ===~%~%")
    (format t "p         : ~a~%" (polynomial->string p))
    (format t "q         : ~a~%" (polynomial->string q))
    (format t "p + q     : ~a~%" (polynomial->string sum))
    (format t "p - q     : ~a~%" (polynomial->string diff))
    (format t "2 * p     : ~a~%" (polynomial->string scaled))
    (format t "degree(p) : ~a~%" (polynomial-degree p))
    (format t "p(0)      : ~a~%" (polynomial-evaluate p 0))
    (format t "p(1)      : ~a~%" (polynomial-evaluate p 1))
    (format t "p(2)      : ~a~%" (polynomial-evaluate p 2))
    (format t "indef     : ~a~%" (integral->string indef))
    (format t "def       : ~a~%" (integral->string def))
    (format t "pi-int    : ~a~%" (integral->string pi-int))
    (format t "~%===========================================~%")))

;; Uncomment the next line to run the smoke test on load:
;; (run-smoke-test)
