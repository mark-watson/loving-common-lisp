;;;; differentiation.lisp — Symbolic differentiation of polynomials
;;;;
;;;; Implements the standard polynomial differentiation rules:
;;;;
;;;;   Power rule:   d/dx(c · x^n)  = n·c · x^(n-1)   (n ≥ 1)
;;;;                 d/dx(constant) = 0
;;;;
;;;;   Sum rule:     d/dx(p + q) = p' + q'
;;;;                 (handled automatically because polynomials are sums of terms)
;;;;
;;;;   Scalar rule:  d/dx(k · p) = k · p'
;;;;                 (provided as a convenience wrapper)
;;;;
;;;; All functions operate on the SYMBOLIC-MATH data structures from data.lisp.
;;;; Load data.lisp before loading this file.

(unless (find-package '#:symbolic-math)
  (load (merge-pathnames "data.lisp" *load-truename*)))

(defpackage #:symbolic-math/diff
  (:use #:cl #:symbolic-math)
  (:export
   ;; Core differentiation
   #:differentiate-term
   #:differentiate
   ;; Higher-order derivatives
   #:differentiate-n
   ;; Convenience
   #:gradient-at
   #:critical-point-p
   ;; Smoke test
   #:run-smoke-test))

(in-package #:symbolic-math/diff)


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 1.  Term-level differentiation  (power rule)
;;;; ─────────────────────────────────────────────────────────────────────────

(defun differentiate-term (term)
  "Apply the power rule to a single monomial TERM.

  d/dx(c · x^n) = n·c · x^(n-1)   for n ≥ 1
  d/dx(c)       = 0                 for n = 0  (returns NIL)

Returns a new sym-term, or NIL when the term differentiates to zero."
  (check-type term sym-term)
  (let ((c (term-coefficient term))
        (v (term-variable    term))
        (n (term-exponent    term)))
    (if (zerop n)
        nil                                     ; constant term → zero, drop it
        (make-term (* n c) v (1- n)))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 2.  Polynomial differentiation  (sum + power)
;;;; ─────────────────────────────────────────────────────────────────────────

(defun differentiate (poly)
  "Differentiate polynomial POLY with respect to its variable.

Uses the power rule on each term and the sum rule across terms.
Returns a new sym-polynomial (the zero polynomial for a constant input).

  Examples:
    (differentiate p)     ; p = 3x² - x + 5  →  6x - 1
    (differentiate (zero-polynomial x))          →  0 (zero polynomial)"
  (check-type poly sym-polynomial)
  (let* ((var          (polynomial-variable poly))
         (diff-terms   (remove nil
                               (mapcar #'differentiate-term
                                       (polynomial-terms poly)))))
    (if (null diff-terms)
        (zero-polynomial var)
        (make-polynomial var diff-terms :domain (polynomial-domain poly)))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 3.  Higher-order derivatives
;;;; ─────────────────────────────────────────────────────────────────────────

(defun differentiate-n (poly n)
  "Return the N-th derivative of polynomial POLY.
N must be a non-negative integer.

  (differentiate-n p 0) → p          (identity)
  (differentiate-n p 1) → p'
  (differentiate-n p 2) → p''
  …"
  (check-type poly sym-polynomial)
  (check-type n (integer 0 *))
  (cond
    ((zerop n) poly)
    (t (differentiate-n (differentiate poly) (1- n)))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 4.  Convenience: numerical gradient & critical-point test
;;;; ─────────────────────────────────────────────────────────────────────────

(defun gradient-at (poly value)
  "Return the numerical value of the derivative of POLY at VALUE.
Differentiates symbolically then evaluates at VALUE.

  (gradient-at p 1)  ; slope of p at x=1"
  (check-type poly sym-polynomial)
  (check-type value real)
  (polynomial-evaluate (differentiate poly) value))

(defun critical-point-p (poly value &key (tolerance 1.0d-9))
  "Return T if VALUE is (numerically) a critical point of POLY,
i.e. |p'(VALUE)| < TOLERANCE.

  (critical-point-p p 1/6)  ; is x=1/6 a critical point of p?"
  (< (abs (gradient-at poly value)) tolerance))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 5.  Smoke test
;;;; ─────────────────────────────────────────────────────────────────────────

(defun run-smoke-test ()
  "Run a quick sanity check on the differentiation functions, printing results."
  (let* ((x  (make-variable 'x))

         ;; p = 3x² - x + 5
         (p  (make-polynomial x (list (make-term  3 x 2)
                                      (make-term -1 x 1)
                                      (make-term  5 x 0))))

         ;; p' = 6x - 1
         (dp  (differentiate p))

         ;; p'' = 6
         (ddp (differentiate dp))

         ;; p''' = 0  (zero polynomial)
         (dddp (differentiate ddp))

         ;; q = x^4 - 2x^3 + x
         (q (make-polynomial x (list (make-term  1 x 4)
                                     (make-term -2 x 3)
                                     (make-term  1 x 1))))
         ;; q' = 4x^3 - 6x^2 + 1
         (dq  (differentiate q))

         ;; q'' = 12x^2 - 12x
         (ddq (differentiate-n q 2))

         ;; Critical point of p: p'(x)=0 → 6x-1=0 → x=1/6
         (cp  (/ 1 6)))

    (format t "~%=== Differentiation Smoke Test ===~%~%")
    (format t "p            : ~a~%" (polynomial->string p))
    (format t "p'           : ~a~%" (polynomial->string dp))
    (format t "p''          : ~a~%" (polynomial->string ddp))
    (format t "p'''         : ~a~%" (polynomial->string dddp))
    (format t "~%")
    (format t "q            : ~a~%" (polynomial->string q))
    (format t "q'           : ~a~%" (polynomial->string dq))
    (format t "q'' (via n=2): ~a~%" (polynomial->string ddq))
    (format t "~%")
    (format t "gradient-at(p, 0)      : ~a  (expected -1)~%"
            (gradient-at p 0))
    (format t "gradient-at(p, 1)      : ~a  (expected  5)~%"
            (gradient-at p 1))
    (format t "critical-point-p(p,1/6): ~a  (expected T)~%"
            (critical-point-p p cp))
    (format t "critical-point-p(p,0)  : ~a  (expected NIL)~%"
            (critical-point-p p 0))
    (format t "~%==================================~%")))
