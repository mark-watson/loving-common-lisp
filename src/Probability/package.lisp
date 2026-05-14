;;;; package.lisp — public API for the probability library

(defpackage #:probability
  (:use #:cl)
  (:export
   ;; Bayesian inference
   #:make-bayes-model
   #:update
   #:posterior
   #:posteriors
   #:maximum-a-posteriori

   ;; Correlation helpers
   #:pearson-r
   #:spearman-rho
   #:correlation-matrix

   ;; Frequentist toolkit
   #:z-score
   #:z-test-proportion
   #:chi-squared-test
   #:confidence-interval-proportion

   ;; Worked examples
   #:run-medical-example
   #:run-frequentist-demo))
