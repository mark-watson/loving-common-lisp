;;;; frequentist.lisp — Frequentist hypothesis-testing toolkit
;;;;
;;;; Provides the core tools of null-hypothesis significance testing:
;;;; z-tests, chi-squared tests, and confidence intervals.
;;;;
;;;; IMPORTANT CAVEAT (repeated deliberately):
;;;; A small p-value tells you the observed data would be unlikely
;;;; *if* the null hypothesis were true.  It does NOT tell you the
;;;; probability that the null hypothesis is false, nor the probability
;;;; that your alternative hypothesis is true.  Confusing these is the
;;;; single most common error in applied statistics.

(in-package #:probability)

;;; ======================================================================
;;;  Normal CDF approximation (Abramowitz & Stegun 26.2.17)
;;;
;;;  Maximum absolute error: 1.5 × 10⁻⁷
;;;  Input: z (real number)
;;;  Output: Φ(z) = P(Z ≤ z) for Z ~ N(0,1)
;;; ======================================================================

(defun phi-approx (z)
  "Approximate the standard normal CDF Φ(z) using the
Abramowitz & Stegun 26.2.17 rational approximation."
  (let* ((p  0.2316419d0)
         (b1 0.319381530d0)
         (b2 -0.356563782d0)
         (b3 1.781477937d0)
         (b4 -1.821255978d0)
         (b5 1.330274429d0)
         (az (abs (float z 1.0d0)))
         (t-val (/ 1.0d0 (+ 1.0d0 (* p az))))
         (pdf (/ (exp (* -0.5d0 az az))
                 (sqrt (* 2.0d0 pi))))
         (cdf (- 1.0d0
                 (* pdf
                    (+ (* b1 t-val)
                       (* b2 (expt t-val 2))
                       (* b3 (expt t-val 3))
                       (* b4 (expt t-val 4))
                       (* b5 (expt t-val 5)))))))
    (if (>= z 0.0d0) cdf (- 1.0d0 cdf))))

;;; ======================================================================
;;;  Chi-squared CDF approximation (Wilson–Hilferty, 1931)
;;;
;;;  Transforms chi² to an approximately standard-normal variate:
;;;     z ≈ ((χ²/k)^(1/3) − (1 − 2/(9k))) / sqrt(2/(9k))
;;;  then uses phi-approx.  Good for df ≥ 3; acceptable for df ≥ 1.
;;; ======================================================================

(defun chi-squared-cdf (x df)
  "Approximate the chi-squared CDF P(X ≤ x) with DF degrees of freedom,
using the Wilson–Hilferty normal approximation."
  (when (<= x 0.0d0) (return-from chi-squared-cdf 0.0d0))
  (let* ((k   (float df 1.0d0))
         (term (/ 2.0d0 (* 9.0d0 k)))
         (z    (/ (- (expt (/ x k) (/ 1.0d0 3.0d0))
                     (- 1.0d0 term))
                  (sqrt term))))
    (phi-approx z)))

;;; ======================================================================
;;;  Z-score
;;; ======================================================================

(defun z-score (observed expected std-dev)
  "Compute the standard z-score: (OBSERVED − EXPECTED) / STD-DEV."
  (assert (plusp std-dev) () "STD-DEV must be positive.")
  (/ (- (float observed 1.0d0) (float expected 1.0d0))
     (float std-dev 1.0d0)))

;;; ======================================================================
;;;  One-sample z-test for a proportion
;;; ======================================================================

(defun z-test-proportion (successes n hypothesised-p)
  "One-sample z-test for a binomial proportion.
Tests H₀: p = HYPOTHESISED-P against H₁: p ≠ HYPOTHESISED-P (two-tailed).

Returns two values: Z-SCORE and P-VALUE."
  (let* ((p0    (float hypothesised-p 1.0d0))
         (n     (float n 1.0d0))
         (p-hat (/ (float successes 1.0d0) n))
         (se    (sqrt (/ (* p0 (- 1.0d0 p0)) n)))
         (z     (/ (- p-hat p0) se))
         (p-val (* 2.0d0 (- 1.0d0 (phi-approx (abs z))))))
    (values z (min p-val 1.0d0))))

;;; ======================================================================
;;;  Pearson's chi-squared test (goodness-of-fit)
;;; ======================================================================

(defun chi-squared-test (observed expected)
  "Pearson's chi-squared goodness-of-fit test.
OBSERVED and EXPECTED are equal-length lists of non-negative counts.

Returns three values: CHI-SQUARED statistic, DF, and P-VALUE.
H₀: observed counts follow the expected distribution."
  (assert (= (length observed) (length expected)) ()
          "OBSERVED and EXPECTED must have the same length.")
  (let* ((chi2 (loop for o in observed
                     for e in expected
                     sum (let ((o-f (float o 1.0d0))
                               (e-f (float e 1.0d0)))
                           (if (zerop e-f) 0.0d0
                               (/ (expt (- o-f e-f) 2) e-f)))))
         (df   (1- (length observed)))
         (p-val (- 1.0d0 (chi-squared-cdf chi2 df))))
    (values chi2 df (max p-val 0.0d0))))

;;; ======================================================================
;;;  Wilson score confidence interval for a proportion
;;; ======================================================================

(defun z-critical (confidence)
  "Return the z* critical value for a two-sided confidence level.
Uses a small lookup + linear interpolation for common levels;
falls back to a Newton-step refinement of the A&S approximation."
  ;; Common values hard-coded for accuracy.
  (let ((table '((0.90d0 . 1.6449d0)
                 (0.95d0 . 1.9600d0)
                 (0.99d0 . 2.5758d0))))
    (let ((entry (assoc (float confidence 1.0d0) table
                        :test (lambda (a b) (< (abs (- a b)) 1.0d-6)))))
      (if entry
          (cdr entry)
          ;; Fallback: solve Φ(z) = (1+confidence)/2 by bisection.
          (let* ((target (/ (+ 1.0d0 (float confidence 1.0d0)) 2.0d0))
                 (lo 0.0d0) (hi 5.0d0))
            (dotimes (_ 60)
              (let ((mid (/ (+ lo hi) 2.0d0)))
                (if (< (phi-approx mid) target)
                    (setf lo mid)
                    (setf hi mid))))
            (/ (+ lo hi) 2.0d0))))))

(defun confidence-interval-proportion (successes n &key (confidence 0.95d0))
  "Wilson score confidence interval for a binomial proportion.
More accurate than the Wald (normal-approximation) interval,
especially for small samples or extreme proportions.

Returns two values: LOWER and UPPER bounds."
  (let* ((n    (float n 1.0d0))
         (p    (/ (float successes 1.0d0) n))
         (z    (z-critical confidence))
         (z2   (* z z))
         (denom (+ 1.0d0 (/ z2 n)))
         (centre (/ (+ p (/ z2 (* 2.0d0 n))) denom))
         (margin (/ (* z (sqrt (+ (/ (* p (- 1.0d0 p)) n)
                                   (/ z2 (* 4.0d0 n n)))))
                    denom)))
    (values (max 0.0d0 (- centre margin))
            (min 1.0d0 (+ centre margin)))))
