;;;; symbolic-math.asd — ASDF system definition for the symbolic math library

(asdf:defsystem #:symbolic-math
  :description "A small, purely functional symbolic mathematics library for Common Lisp: variables, constants, monomials, polynomials, symbolic differentiation, and symbolic integration."
  :author "Mark Watson"
  :license "Apache-2.0"
  :serial t
  :components ((:file "data")
               (:file "differentiation")
               (:file "integration")))
