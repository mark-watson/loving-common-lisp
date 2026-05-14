;;;; examples/medical.lisp — Medical-diagnosis worked example
;;;;
;;;; Scenario
;;;; --------
;;;; A rare disease affects 0.1 % of the population.
;;;; A screening test has:
;;;;   • Sensitivity (true-positive rate):  99 %
;;;;   • False-positive rate:                5 %
;;;;
;;;; Question: if a patient tests positive, what is the probability
;;;; that they actually have the disease?
;;;;
;;;; Intuition says "very high" — but Bayes' Theorem reveals it is
;;;; only about 1.9 %.  This example demonstrates that result and
;;;; then uses the correlation module to show that test results and
;;;; actual diagnoses can appear correlated while the individual
;;;; predictive value remains low.

(in-package #:probability)

;;; ---------- parameters ----------------------------------------------------

(defparameter *prevalence*        0.001d0  "P(disease)")
(defparameter *sensitivity*       0.99d0   "P(positive | disease)")
(defparameter *false-positive-rate* 0.05d0 "P(positive | healthy)")

;;; ---------- Bayesian analysis --------------------------------------------

(defun medical-likelihood (hypothesis)
  "Return P(positive-test | HYPOTHESIS)."
  (ecase hypothesis
    (:disease *sensitivity*)
    (:healthy *false-positive-rate*)))

(defun run-bayesian-analysis ()
  "Compute posterior probability of disease given a positive test."
  (let* ((prior (make-bayes-model `((:disease . ,*prevalence*)
                                    (:healthy . ,(- 1.0d0 *prevalence*)))))
         (updated (update prior :positive-test #'medical-likelihood)))
    (format t "~%=== Bayesian Analysis: Medical Screening Test ===~%")
    (format t "Prior probabilities:~%")
    (dolist (p (posteriors prior))
      (format t "  P(~A) = ~,4F~%" (car p) (cdr p)))
    (format t "~%After a POSITIVE test result:~%")
    (dolist (p (posteriors updated))
      (format t "  P(~A | positive) = ~,4F  (~,2F %)~%"
              (car p) (cdr p) (* 100.0d0 (cdr p))))
    (format t "~%MAP hypothesis: ~A~%" (car (maximum-a-posteriori updated)))
    (format t "~%Key insight: despite 99% sensitivity, a positive test~%")
    (format t "only yields about 1.9% probability of disease because the~%")
    (format t "disease is so rare (0.1% prevalence).  This is exactly~%")
    (format t "the kind of counter-intuitive result Bayes' Theorem reveals.~%")
    updated))

;;; ---------- Correlation analysis on synthetic population ------------------

(defun generate-synthetic-population (&optional (n 10000))
  "Generate N individuals; return (TEST-RESULTS . ACTUAL-DIAGNOSES) as 0/1 lists."
  ;; Simple PRNG-based simulation.
  (let ((tests     '())
        (diagnoses '()))
    (dotimes (_ n)
      (let* ((has-disease (< (random 1.0d0) *prevalence*))
             (test-positive
               (if has-disease
                   (< (random 1.0d0) *sensitivity*)
                   (< (random 1.0d0) *false-positive-rate*))))
        (push (if test-positive 1.0d0 0.0d0) tests)
        (push (if has-disease  1.0d0 0.0d0) diagnoses)))
    (cons (nreverse tests) (nreverse diagnoses))))

(defun run-correlation-analysis ()
  "Generate a synthetic population and compute Pearson-r between test
results and actual disease status."
  (let* ((pop (generate-synthetic-population 100000))
         (tests     (car pop))
         (diagnoses (cdr pop))
         (r (pearson-r tests diagnoses)))
    (format t "~%=== Correlation Analysis (N = ~D) ===~%" (length tests))
    (format t "Pearson r(test-result, disease) = ~,4F~%" r)
    (format t "~%This positive correlation is real but modest.  It shows~%")
    (format t "that the test result and disease status are associated,~%")
    (format t "but the correlation coefficient alone cannot tell you the~%")
    (format t "probability that any *individual* patient is sick — that~%")
    (format t "requires Bayesian reasoning with the base rate (prevalence).~%")
    (format t "~%Correlation ≠ causation, and here, even correlation ≠~%")
    (format t "reliable individual prediction.~%")
    r))

;;; ---------- top-level entry point -----------------------------------------

(defun run-medical-example ()
  "Run the full medical-diagnosis worked example."
  (run-bayesian-analysis)
  (run-correlation-analysis)
  (format t "~%=== Done. ===~%")
  (values))
