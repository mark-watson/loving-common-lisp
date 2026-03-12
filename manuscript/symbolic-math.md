# Symbolic Mathematics in Common Lisp

Common Lisp has long been a natural home for symbolic computation. Its homoiconic nature, rich macro system, and first-class support for rational arithmetic make it an ideal language for building systems that manipulate mathematical expressions as data rather than reducing them immediately to floating-point numbers. In this chapter we build a small but complete symbolic mathematics library from scratch. We define data structures for variables, constants, monomials, polynomials, and integrals; implement symbolic differentiation using the power rule; and implement symbolic integration using the reverse power rule together with the Fundamental Theorem of Calculus. The result is a readable, testable, purely functional library that illuminates both the mathematics and the Lisp idioms involved.

## The Data Layer

Every symbolic computation system needs a representation for the objects it manipulates. Before we can differentiate or integrate anything we must decide how to store variables, constants, terms, polynomials, and integrals. The choices made here cascade through the rest of the system, so it is worth studying them carefully.

### Design Philosophy

The library is deliberately **CLOS-free**. Every type is defined with `defstruct`, which gives us lightweight value structs with automatic constructors, slot accessors, and copy functions. All arithmetic operations return fresh structs; no existing object is ever mutated. This immutable, functional style keeps the code easy to reason about and makes it straightforward to use results as inputs to further computation without defensive copying.

The entire data layer lives in the `SYMBOLIC-MATH` package and is loaded from a single file, `data.lisp`. The package exports every public name — constructors, predicates, accessors, and helpers — so that the differentiation and integration layers can import only what they need with `(:use #:cl #:symbolic-math)`.

### Variables and Constants

The two most primitive objects in the system are symbolic variables (unknowns like *x* or *t*) and named constants (fixed values like π or *e*). The following listing defines both structs together with their constructors, predicates, and a numeric-evaluation helper for constants.

```lisp
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

(defstruct (sym-constant (:conc-name constant-))
  "A named mathematical constant.
VALUE may be a Common Lisp number, :pi, or :e (Euler's number)."
  (name  nil :type symbol)
  (value nil))                          ; number | :pi | :e

(defun make-constant (name value)
  "Create a named constant with NAME (symbol) and VALUE.
VALUE may be a real number, :pi, or :e."
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
```

`sym-variable` stores a Lisp symbol as the variable name and a keyword denoting its domain. The `(:conc-name variable-)` option tells `defstruct` to prefix all generated accessor names with `variable-`, so the name slot is reached via `variable-name` rather than the default `sym-variable-name`. The `make-variable` wrapper validates its arguments with `check-type` and `assert` before delegating to the raw struct constructor, ensuring that only well-formed objects enter the system.

`sym-constant` follows the same pattern but its `value` slot is more flexible: it may hold a Common Lisp real number, the keyword `:pi`, or the keyword `:e`. The `constant-numeric-value` function resolves any of these to a `double-float` on demand using a short `cond` dispatch. Storing the symbolic name alongside the numeric value means that bounds such as π can be displayed in human-readable form (as shown later in the integral display code) while still being convertible to a floating-point number when a definite integral must be evaluated numerically.

### Monomials (Terms)

A monomial — a single product of a coefficient, a variable, and a non-negative integer power — is the atom from which polynomials are assembled. The `sym-term` struct and its helpers are shown below.

```lisp
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
    (make-term 5  x 0)   ; constant 5"
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
```

The exponent type specifier `(integer 0 *)` in the struct definition is not merely documentation — Common Lisp compilers can use it to generate better code, and the runtime will signal a type error if anything other than a non-negative integer is stored there. The `term-negate` and `term-scale` functions are pure: they build new `sym-term` structs rather than mutating the original. This purity matters because the polynomial arithmetic functions rely on these helpers and expect their inputs to remain unchanged.

`term->string` uses a three-branch `cond` to handle the three visually distinct cases: a constant term (exponent zero, print only the number), a linear term (exponent one, omit the `^1`), and a higher-degree term (print the full `c v^e` form). The `symbol-name` call converts the Lisp symbol stored in the variable to a plain string so that `format` does not print an unexpected package prefix.

### Polynomials

A polynomial is an ordered collection of monomials in a single variable. The implementation stores terms in descending exponent order and automatically combines like terms on every construction. This canonical form means that two polynomials representing the same mathematical object will always have the same in-memory representation.

```lisp
(defstruct (sym-polynomial (:conc-name polynomial-))
  "A polynomial in one VARIABLE represented as an ordered list of TERMS.
Terms are kept in descending order by exponent.
DOMAIN is :real (default) or :complex."
  (variable nil)                       ; sym-variable
  (terms    '() :type list)            ; list of sym-term, descending exponent
  (domain   :real :type keyword))

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
  "Create a polynomial in VARIABLE from a list of TERMS.
Like-exponent terms are combined automatically; zero-coefficient terms are dropped.
Result terms are stored in descending exponent order."
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

(defun polynomial-degree (poly)
  "Return the degree (highest exponent) of POLY.
Returns -1 for the zero polynomial (no terms)."
  (check-type poly sym-polynomial)
  (if (null (polynomial-terms poly))
      -1
      (term-exponent (first (polynomial-terms poly)))))

(defun polynomial-add (p q)
  "Return P + Q as a new polynomial."
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

(defun polynomial-evaluate (poly value)
  "Evaluate POLY at VALUE by substituting the variable."
  (check-type poly sym-polynomial)
  (check-type value real)
  (reduce #'+
          (polynomial-terms poly)
          :key (lambda (term)
                 (* (term-coefficient term)
                    (expt value (term-exponent term))))
          :initial-value 0))

(defun polynomial->string (poly)
  "Return a human-readable string for POLY, e.g. '3x^2 + -1x^1 + 5'."
  (check-type poly sym-polynomial)
  (if (null (polynomial-terms poly))
      "0"
      (format nil "~{~a~^ + ~}"
              (mapcar #'term->string (polynomial-terms poly)))))
```

The private helper `%sort-and-combine-terms` performs the normalization work in two passes. The first pass accumulates coefficients into a hash table keyed by exponent, so any duplicate exponents are automatically merged. The second pass walks the table, drops entries whose total coefficient is zero, and collects the survivors as `(exponent . coefficient)` cons cells. The public `make-polynomial` then sorts those pairs in descending order and rebuilds proper `sym-term` structs.

Polynomial arithmetic is elegantly simple because `make-polynomial` already normalizes. Addition merely appends the two term lists and lets the constructor handle combination and sorting. Subtraction negates `q` (by mapping `term-negate` over its terms) then calls addition. Scaling maps `term-scale` over all terms. Evaluation uses `reduce` with an accumulating `+` over all terms, computing each term's contribution as `c * value^n` with Common Lisp's built-in `expt`.

### Integrals as Data

The last data structure in the layer represents an unevaluated integral expression — either indefinite (no bounds) or definite (with a lower and upper bound). Storing the integral as data rather than immediately computing it allows the system to display it symbolically before choosing to evaluate it numerically.

```lisp
(defstruct (sym-integral (:conc-name integral-))
  "Represents a definite or indefinite integral.
INTEGRAND — the expression being integrated
VARIABLE  — the variable of integration
LOWER     — lower bound (number, sym-constant, or NIL for indefinite)
UPPER     — upper bound (number, sym-constant, or NIL for indefinite)"
  (integrand nil)
  (variable  nil)
  (lower     nil)
  (upper     nil))

(defun make-integral (integrand variable &key lower upper)
  "Create a symbolic integral.
Either both LOWER and UPPER must be provided, or neither."
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
  "Return a human-readable Unicode string for INTEGRAL.
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
```

The `make-integral` constructor enforces the constraint that bounds must come in pairs: you may supply both `lower` and `upper`, or neither, but supplying only one is a programming error caught at runtime with `assert`. This is a small but valuable design choice — it prevents silently constructing a malformed integral whose lower bound is non-nil but whose upper bound is nil.

`integral->string` uses the Unicode integral sign `∫` directly in a format string, which works in any modern Common Lisp environment whose source files are saved in UTF-8. The body of the integral is rendered by dispatching on the integrand's type: if it is a `sym-polynomial` the existing `polynomial->string` is used; anything else falls back to `format`'s default `~a` printer. Bounds are rendered by the private `%bound->string` helper, which knows how to convert both plain numbers and `sym-constant` objects to strings.

### Data Layer Smoke Test

The file closes with a self-contained smoke test that exercises all five types together and prints human-readable results to standard output.

```lisp
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
         (sum    (polynomial-add p q))
         (diff   (polynomial-subtract p q))
         (scaled (polynomial-scale p 2))
         (indef  (make-integral p x))
         (def    (make-integral p x :lower 0 :upper 1))
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
```

The smoke test is written as a single large `let*` binding so that every intermediate result is named and visible. All variables, polynomials, sums, differences, scaled copies, and integral objects are constructed in the binding clauses; the body is purely a sequence of `format` calls that print them. This layout makes it trivial to inspect the test in a REPL: evaluate any sub-expression from the binding list and examine the result interactively.

Running `(run-smoke-test)` after loading `data.lisp` produces output like the following, confirming that polynomial arithmetic, degree computation, evaluation, and integral display all work correctly:

```
=== Symbolic Math Data Layer Smoke Test ===

p         : 3x^2 + -1x^1 + 5
q         : 1x^1 + 2
p + q     : 3x^2 + 7
p - q     : 3x^2 + -2x^1 + 3
2 * p     : 6x^2 + -2x^1 + 10
degree(p) : 2
p(0)      : 5
p(1)      : 7
p(2)      : 15
indef     : ∫(3x^2 + -1x^1 + 5) dx
def       : ∫[0,1](3x^2 + -1x^1 + 5) dx
pi-int    : ∫[0,pi](3x^2 + -1x^1 + 5) dx

===========================================
```

## Symbolic Differentiation

With the data layer in place we can build the differentiation engine. Polynomial differentiation is driven by two classical rules: the **power rule** for individual terms, and the **sum rule** for multi-term polynomials. Because our polynomial representation is already a list of terms, the sum rule requires no special code — it is a natural consequence of mapping the power rule over every element of the list.

The differentiation package lives in `differentiation.lisp` and declares itself as `SYMBOLIC-MATH/DIFF`, using `(:use #:cl #:symbolic-math)` to inherit the full data layer.

### The Power Rule on a Single Term

The power rule states that `d/dx(c · xⁿ) = n·c · x^(n−1)` for `n ≥ 1`, and that the derivative of a constant is zero. The `differentiate-term` function encodes this directly.

```lisp
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
```

The implementation is a two-branch `if`. When the exponent is zero the term is a constant and its derivative is zero; returning `nil` rather than a zero-coefficient term lets the caller use `remove nil` to discard it cleanly without an extra zero in the term list. For any positive exponent the function multiplies the coefficient by the exponent (`n·c`) and decrements the exponent by one (`n−1`), producing a new `sym-term` via `make-term`.

Because the function is pure — it reads three slots from an existing term and constructs a brand-new one — it is trivially testable in isolation. It also composes naturally with `mapcar` since it is a function from one term to one term-or-nil, which is exactly what polynomial-level differentiation needs.

### Differentiating a Polynomial

The `differentiate` function applies `differentiate-term` to every term in a polynomial, discards the nil results, and re-assembles the survivors into a new polynomial.

```lisp
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
```

The sum rule is implicit: `mapcar` applies `differentiate-term` independently to every term, and `remove nil` strips out the constant-term derivatives that returned nil. If all terms were constant the surviving list is empty and `zero-polynomial` is returned explicitly rather than passing an empty list to `make-polynomial`. This edge case matters: differentiating a constant polynomial must yield the zero polynomial, and spelling it out explicitly is clearer than relying on `make-polynomial`'s behaviour for an empty input.

### Higher-Order Derivatives

Taking repeated derivatives is expressed as a simple recursion. `differentiate-n` applies `differentiate` *n* times by peeling one application off per recursive call.

```lisp
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
```

The base case `(zerop n)` returns the polynomial unchanged, encoding the mathematical identity that the zeroth derivative of a function is the function itself. The recursive case differentiates once and recurses with `n-1`. Because polynomials have finite degree, the recursion always terminates: at most `(degree poly + 1)` calls are needed before the result is the zero polynomial, and further applications of `differentiate` to the zero polynomial keep returning the zero polynomial.

### Numerical Gradient and Critical Points

Two convenience functions bridge the symbolic and numeric worlds. `gradient-at` differentiates a polynomial symbolically and then evaluates the result at a specific point, yielding the slope of the curve at that *x*-value. `critical-point-p` uses `gradient-at` to test whether the derivative is numerically zero at a given point.

```lisp
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
```

`gradient-at` is a simple composition: differentiate then evaluate. The symbolic step ensures the derivative is exact; the numeric step converts the exact result to a number. `critical-point-p` compares the absolute value of the gradient against a configurable tolerance, defaulting to `1.0d-9`. Using a tolerance rather than testing for exact equality is important because floating-point arithmetic may introduce small rounding errors even when the true derivative is zero. The `&key` parameter makes the tolerance easy to adjust without changing the call site.

### Differentiation Smoke Test

The differentiation module includes its own smoke test to verify all four exported functions.

```lisp
(defun run-smoke-test ()
  "Run a quick sanity check on the differentiation functions, printing results."
  (let* ((x  (make-variable 'x))

         ;; p = 3x² - x + 5
         (p  (make-polynomial x (list (make-term  3 x 2)
                                      (make-term -1 x 1)
                                      (make-term  5 x 0))))
         (dp  (differentiate p))          ; p' = 6x - 1
         (ddp (differentiate dp))         ; p'' = 6
         (dddp (differentiate ddp))       ; p''' = 0

         ;; q = x^4 - 2x^3 + x
         (q (make-polynomial x (list (make-term  1 x 4)
                                     (make-term -2 x 3)
                                     (make-term  1 x 1))))
         (dq  (differentiate q))          ; q' = 4x^3 - 6x^2 + 1
         (ddq (differentiate-n q 2))      ; q'' = 12x^2 - 12x

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
```

The test covers differentiation of a quadratic through to the zero polynomial, a degree-4 polynomial differentiated both step-by-step and with `differentiate-n`, and two critical-point tests — one at the true critical point `x = 1/6` and one at `x = 0` which is not a critical point. Embedding the expected values in the format strings (`expected -1`, `expected T`, etc.) means the output is self-documenting: a developer reading the terminal output can immediately see whether each result is correct without consulting a separate specification.

## Symbolic Integration

Integration is the inverse of differentiation. Where differentiation reduces degree by one, integration raises it. The integration package in `integration.lisp` implements the **reverse power rule** for individual terms, extends it to polynomials via the sum rule, evaluates definite integrals numerically using the **Fundamental Theorem of Calculus**, and provides helpers for constructing `sym-integral` shell objects.

The package is `SYMBOLIC-MATH/INTEG`, again using `(:use #:cl #:symbolic-math)`.

### The Reverse Power Rule on a Single Term

The reverse power rule states that `∫ c · xⁿ dx = (c / (n+1)) · x^(n+1)`. Because *n* is always a non-negative integer here, `n+1` is always positive, so division is always well-defined and no special case is needed.

```lisp
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
```

The division `(/ c new-exp)` is the algebraic heart of the function. In Common Lisp, dividing an integer by another integer produces an exact rational number rather than a float. Integrating the term `(make-term 3 x 2)` (representing 3x²) yields the new coefficient `3/3 = 1`; integrating `(make-term -1 x 1)` (representing −x) yields `-1/2`. These rationals propagate unchanged through subsequent operations, so no precision is lost until the caller explicitly requests a float — a significant advantage over languages that default to floating-point division.

Unlike `differentiate-term`, this function never returns nil because every term, including a constant term with exponent zero, integrates to a non-zero result. A constant `5` integrates to `5x`, so there is no zero-result case to handle.

### The Antiderivative of a Polynomial

`integrate` maps `integrate-term` over every term of the polynomial and assembles the results into a new polynomial of degree `(degree poly + 1)`.

```lisp
(defun integrate (poly)
  "Return the antiderivative of polynomial POLY (without the constant of
integration +C).  Applies the reverse power rule to each term.

  Examples:
    poly  = 3x² - x + 5
    ∫poly = x³ - (1/2)x² + 5x

  Returns a new sym-polynomial of degree (degree(POLY) + 1)."
  (check-type poly sym-polynomial)
  (let* ((var  (polynomial-variable poly))
         (new-terms (mapcar #'integrate-term (polynomial-terms poly))))
    (if (null new-terms)
        (zero-polynomial var)
        (make-polynomial var new-terms :domain (polynomial-domain poly)))))
```

The structure mirrors `differentiate` almost exactly, with `integrate-term` replacing `differentiate-term` and `remove nil` omitted because `integrate-term` never returns nil. The zero-polynomial edge case is still handled: integrating the zero polynomial (no terms) returns the zero polynomial rather than passing an empty list to `make-polynomial`. Notice that the constant of integration `+C` is deliberately omitted. In an indefinite integration context `+C` represents an entire family of functions; adding a zero-exponent term for it would require an arbitrary choice for the constant's coefficient. By leaving it out, the function returns the canonical antiderivative, and callers can add their own constant term if needed.

### Definite Integral Evaluation

The Fundamental Theorem of Calculus reduces a definite integral to two polynomial evaluations: `∫[a,b] f dx = F(b) − F(a)`, where `F` is the antiderivative of `f`.

```lisp
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
    (evaluate-definite p 0 1)
    (evaluate-definite p 0 (make-constant 'pi :pi))"
  (check-type poly sym-polynomial)
  (let* ((F  (integrate poly))
         (a  (coerce (%resolve-bound lower) 'double-float))
         (b  (coerce (%resolve-bound upper) 'double-float)))
    (- (polynomial-evaluate F b)
       (polynomial-evaluate F a))))
```

`%resolve-bound` is a small dispatch function that converts whichever bound type was supplied — plain number, `sym-constant`, or an unexpected type — to a numeric value or signals an informative error. The two `coerce` calls in `evaluate-definite` ensure the bounds are `double-float` before passing them to `polynomial-evaluate`, which prevents mixed exact/floating-point arithmetic from producing surprising results. The final subtraction `F(b) - F(a)` is the Fundamental Theorem in code, and its simplicity is a direct reflection of the theorem's elegance.

### Iterated Antiderivatives

Just as `differentiate-n` applies differentiation *n* times, `integrate-n` applies integration *n* times.

```lisp
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
```

The recursion mirrors `differentiate-n` in structure: base case returns the polynomial unchanged; recursive case integrates once and decrements `n`. Because each application of `integrate` raises the degree by exactly one, `integrate-n` on a polynomial of degree `d` produces a polynomial of degree `d + n`. The rational arithmetic in `integrate-term` becomes especially valuable here: integrating twice starting from integer coefficients yields rational coefficients that are still exact, whereas floating-point arithmetic would accumulate rounding error with each pass.

### Integral Shell Constructors

Two thin wrappers make it convenient to package a polynomial together with its bounds into the `sym-integral` data structure defined in the data layer.

```lisp
(defun make-indefinite-integral (poly)
  "Create a sym-integral wrapping POLY (indefinite form).
The stored integrand is POLY itself.

  Rendering:  ∫(expr) dx"
  (check-type poly sym-polynomial)
  (make-integral poly (polynomial-variable poly)))

(defun make-definite-integral (poly lower upper)
  "Create a sym-integral wrapping POLY with explicit bounds LOWER and UPPER.
LOWER and UPPER may be real numbers or sym-constant objects.

  Rendering:  ∫[a,b](expr) dx"
  (check-type poly sym-polynomial)
  (make-integral poly (polynomial-variable poly) :lower lower :upper upper))
```

These functions exist purely for ergonomics. Without them, a caller would need to separately extract the polynomial's variable and pass it as the second argument to `make-integral` — a minor but unnecessary repetition. By encapsulating that extraction, the wrappers let calling code read more naturally: `(make-indefinite-integral p)` rather than `(make-integral p (polynomial-variable p))`. Note that these constructors store the original polynomial as the integrand; they do not pre-compute the antiderivative. Evaluation is deferred to an explicit call to `evaluate-definite` or `integrate`.

### Integration Smoke Test

The integration smoke test verifies each exported function and echoes expected values inline.

```lisp
(defun run-smoke-test ()
  "Run a quick sanity check on the integration functions, printing results."
  (let* ((x  (make-variable 'x))
         (pi-c (make-constant 'pi :pi))

         ;; p = 3x² - x + 5
         (p   (make-polynomial x (list (make-term  3 x 2)
                                       (make-term -1 x 1)
                                       (make-term  5 x 0))))
         (ip  (integrate p))          ; x³ - (1/2)x² + 5x
         (iip (integrate-n p 2))      ; (1/4)x⁴ - (1/6)x³ + (5/2)x²

         ;; q = 6x + 2
         (q   (make-polynomial x (list (make-term 6 x 1)
                                       (make-term 2 x 0))))
         (iq  (integrate q))          ; 3x² + 2x

         (def-01    (evaluate-definite p 0 1))   ; expected 5.5
         (def-q-01  (evaluate-definite q 0 1))   ; expected 5.0
         (def-pi    (evaluate-definite p 0 pi-c))

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
```

The test checks the antiderivative of `p` (a quadratic), the second antiderivative of `p`, the antiderivative of `q` (a linear polynomial), two numerical definite integrals with known exact values (5.5 and 5.0), a definite integral with a `sym-constant` bound (`π`), and the string rendering of all three integral shell variants. The `~f` directive for the `π`-bounded result prints a floating-point number whose exact digits will vary by implementation but whose value should be approximately `102.olean` — a sanity check that the numeric path through `sym-constant` works end-to-end.

## Wrap Up

In this chapter we built a three-file symbolic mathematics library in Common Lisp that demonstrates how a language designed for symbolic computation can express mathematical concepts cleanly and correctly.

The **data layer** (`data.lisp`) showed how `defstruct` provides a lightweight, functional alternative to CLOS for domain objects. By enforcing invariants at construction time with `check-type` and `assert`, we ensured that ill-formed objects never enter the system. The canonical-form polynomial representation — terms always sorted by descending exponent with like terms combined — removed the need for normalization logic in every downstream consumer.

The **differentiation layer** (`differentiation.lisp`) demonstrated that the power rule and sum rule can be encoded almost literally in code. `differentiate-term` is a direct transliteration of `d/dx(cxⁿ) = ncx^(n−1)`; `differentiate` applies it via `mapcar` and `remove nil`, and the sum rule emerges automatically from the polynomial's list structure. Higher-order derivatives and numerical gradient evaluation required only a handful of additional lines.

The **integration layer** (`integration.lisp`) mirrored the differentiation layer but introduced the important advantage of Common Lisp's exact rational arithmetic. Coefficients produced by the reverse power rule — such as the `-1/2` arising from integrating `-x` — are stored as exact rationals, so no precision is lost across multiple integrations. The Fundamental Theorem of Calculus translated into a three-line function that computes an antiderivative symbolically and then performs two numerical evaluations, achieving both symbolic clarity and numeric accuracy.

Taken together, the three files illustrate a broader lesson: when you model a problem domain faithfully as data, the algorithms that manipulate that data often become nearly self-evident. The mathematics drives the code structure rather than the other way around, and the result is software that is simultaneously easier to understand, easier to test, and easier to extend.
