;;;; probability.asd — ASDF system definition for the probability library

(asdf:defsystem #:probability
  :description "A small Common Lisp library for Bayesian inference and correlation analysis."
  :author "Mark Watson"
  :license "Apache-2.0"
  :serial t
  :components ((:file "package")
               (:file "bayes")
               (:file "correlation")
               (:file "frequentist")
               (:module "examples"
                :components ((:file "medical")
                             (:file "frequentist-demo")))))
