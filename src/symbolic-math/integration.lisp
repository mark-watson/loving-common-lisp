;;;; integration.lisp — Symbolic integration of polynomials
;;;;
;;;; Implements the reverse power rule and the fundamental theorem of calculus:
;;;;
;;;;   Reverse power rule:  ∫ c · x^n dx = (c / (n+1)) · x^(n+1) + C
;;;;
;;;;   Sum rule:            ∫ (p + q) dx = ∫p dx + ∫q dx
;;;;                        (handled automatically over the terms list)
;;;;
;;;;   Fundamental theorem: ∫[a,b] f(x) dx = F(b) - F(a)
;;;;                        where F is the antiderivative of f
;;;;
;;;; All functions operate on SYMBOLIC-MATH data structures from data.lisp.
;;;; Load data.lisp before loading this file.

(unless (find-package '#:symbolic-math)
  (load (merge-pathnames "data.lisp" *load-truename*)))

(defpackage #:symbolic-math/integ
  (:use #:cl #:symbolic-math)
  (:export
   ;; Core integration
   #:integrate-term          ; reverse power rule on one sym-term
   #:integrate               ; antiderivative of a sym-polynomial
   ;; Definite integral evaluation
   #:evaluate-definite       ; numeric result of ∫[a,b] p dx
   ;; Higher-order / iterated integrals
   #:integrate-n             ; n-th antiderivative
   ;; sym-integral shell helpers
   #:make-indefinite-integral ; wrap antiderivative in a sym-integral
   #:make-definite-integral   ; wrap antiderivative+bounds in a sym-integral
   ;; Smoke test
   #:run-smoke-test))

(in-package #:symbolic-math/integ)


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 1.  Term-level integration  (reverse power rule)
;;;; ─────────────────────────────────────────────────────────────────────────

(defun integrate-term (term)
  "Apply the reverse power rule to a single monomial TERM.

  ∫ c · x^n dx = (c / (n+1)) · x^(n+1)

Returns a new sym-term.  The constant of integration (+C) is handled at
the polynomial level by the caller.

  Examples:
    (integrate-term (make-term 3 x 2))   ; => (1)x^3  [3/(2+1) = 1]
    (integrate-term (make-term -1 x 1))  ; => (-1/2)x^2
    (integrate-term (make-term 5 x 0))   ; => 5x^1"
  (check-type term sym-term)
  (let* ((c    (term-coefficient term))
         (v    (term-variable    term))
         (n    (term-exponent    term))
         (new-exp  (1+ n))
         (new-coef (/ c new-exp)))          ; exact ratio; CL keeps it rational
    (make-term new-coef v new-exp)))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 2.  Polynomial antiderivative  (sum + reverse power)
;;;; ─────────────────────────────────────────────────────────────────────────

(defun integrate (poly)
  "Return the antiderivative of polynomial POLY (without the constant of
integration +C).  Applies the reverse power rule to each term and the sum
rule across terms.

  Examples:
    poly  = 3x² - x + 5
    ∫poly = x³ - (1/2)x² + 5x

  Returns a new sym-polynomial of degree (degree(POLY) + 1)."
  (check-type poly sym-polynomial)
  (let* ((var  (polynomial-variable poly))
         (new-terms (mapcar #'integrate-term (polynomial-terms poly))))
    (if (null new-terms)
        ;; integral of the zero polynomial is still zero
        (zero-polynomial var)
        (make-polynomial var new-terms :domain (polynomial-domain poly)))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 3.  Definite integral evaluation  (Fundamental Theorem of Calculus)
;;;; ─────────────────────────────────────────────────────────────────────────

(defun %resolve-bound (bound)
  "Internal: convert a bound (number or sym-constant) to a number."
  (cond
    ((null bound)
     (error "Bound is NIL — use evaluate-definite only on definite integrals."))
    ((numberp bound)      bound)
    ((constant-p bound)   (constant-numeric-value bound))
    (t (error "Unrecognised bound type: ~s" bound))))

(defun evaluate-definite (poly lower upper)
  "Evaluate the definite integral ∫[LOWER,UPPER] POLY dx numerically.

LOWER and UPPER may be real numbers or sym-constant objects.

Uses the Fundamental Theorem of Calculus:
  ∫[a,b] f dx = F(b) - F(a)   where F = (integrate poly)

  Examples:
    (evaluate-definite p 0 1)            ; ∫₀¹ p dx
    (evaluate-definite p 0 (make-constant 'pi :pi)) ; ∫₀^π p dx"
  (check-type poly sym-polynomial)
  (let* ((F  (integrate poly))
         (a  (coerce (%resolve-bound lower) 'double-float))
         (b  (coerce (%resolve-bound upper) 'double-float)))
    (- (polynomial-evaluate F b)
       (polynomial-evaluate F a))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 4.  Higher-order (iterated) antiderivatives
;;;; ─────────────────────────────────────────────────────────────────────────

(defun integrate-n (poly n)
  "Return the N-th iterated antiderivative of POLY.
N must be a non-negative integer.

  (integrate-n p 0) → p          (identity)
  (integrate-n p 1) → ∫p dx
  (integrate-n p 2) → ∫∫p dx dx
  …"
  (check-type poly sym-polynomial)
  (check-type n (integer 0 *))
  (cond
    ((zerop n) poly)
    (t (integrate-n (integrate poly) (1- n)))))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 5.  sym-integral shell constructors
;;;; ─────────────────────────────────────────────────────────────────────────

(defun make-indefinite-integral (poly)
  "Create a sym-integral wrapping the antiderivative of POLY (indefinite form).
The stored integrand is POLY itself; the antiderivative is computed lazily
by evaluate-definite / integrate.

  Rendering:  ∫(expr) dx"
  (check-type poly sym-polynomial)
  (make-integral poly (polynomial-variable poly)))

(defun make-definite-integral (poly lower upper)
  "Create a sym-integral wrapping POLY with explicit bounds LOWER and UPPER.
LOWER and UPPER may be real numbers or sym-constant objects.

  Rendering:  ∫[a,b](expr) dx"
  (check-type poly sym-polynomial)
  (make-integral poly (polynomial-variable poly) :lower lower :upper upper))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; 6.  Smoke test
;;;; ─────────────────────────────────────────────────────────────────────────

(defun run-smoke-test ()
  "Run a quick sanity check on the integration functions, printing results."
  (let* ((x  (make-variable 'x))
         (pi-c (make-constant 'pi :pi))

         ;; p = 3x² - x + 5
         (p   (make-polynomial x (list (make-term  3 x 2)
                                       (make-term -1 x 1)
                                       (make-term  5 x 0))))

         ;; ∫p dx = x³ - (1/2)x² + 5x
         (ip  (integrate p))

         ;; ∫²p dx = (1/4)x⁴ - (1/6)x³ + (5/2)x²
         (iip (integrate-n p 2))

         ;; q = 6x + 2
         (q   (make-polynomial x (list (make-term 6 x 1)
                                       (make-term 2 x 0))))

         ;; ∫q dx = 3x² + 2x             ← exact antiderivative
         (iq  (integrate q))

         ;; Definite:  ∫₀¹ p dx  = F(1) - F(0)
         ;;            F(x) = x³ - (1/2)x² + 5x
         ;;            F(1) = 1 - 1/2 + 5 = 5.5
         ;;            F(0) = 0
         ;;            ⇒  5.5
         (def-01    (evaluate-definite p 0 1))

         ;; Definite: ∫₀¹ q dx = [3x² + 2x]₀¹ = 3 + 2 = 5
         (def-q-01  (evaluate-definite q 0 1))

         ;; Definite: ∫₀^π p dx
         (def-pi    (evaluate-definite p 0 pi-c))

         ;; sym-integral shells
         (indef-shell (make-indefinite-integral p))
         (def-shell   (make-definite-integral   p 0 1))
         (pi-shell    (make-definite-integral   p 0 pi-c)))

    (format t "~%=== Integration Smoke Test ===~%~%")

    (format t "p              : ~a~%" (polynomial->string p))
    (format t "∫p dx          : ~a~%" (polynomial->string ip))
    (format t "∫∫p dx dx      : ~a~%" (polynomial->string iip))
    (format t "~%")
    (format t "q              : ~a~%" (polynomial->string q))
    (format t "∫q dx          : ~a~%" (polynomial->string iq))
    (format t "~%")
    (format t "∫₀¹  p dx      : ~a  (expected 5.5)~%"  def-01)
    (format t "∫₀¹  q dx      : ~a  (expected 5.0)~%"  def-q-01)
    (format t "∫₀^π p dx      : ~f~%"                  def-pi)
    (format t "~%")
    (format t "indef shell    : ~a~%" (integral->string indef-shell))
    (format t "def [0,1]      : ~a~%" (integral->string def-shell))
    (format t "def [0,pi]     : ~a~%" (integral->string pi-shell))
    (format t "~%==============================~%")))
