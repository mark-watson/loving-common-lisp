;;;; bayes.lisp — Bayesian inference core
;;;;
;;;; A Bayes model is simply a normalised alist of (hypothesis . probability).
;;;; UPDATE applies Bayes' Theorem:
;;;;
;;;;   P(H | E) = P(E | H) · P(H)  /  Σ_h P(E | h) · P(h)

(in-package #:probability)

;;; ---------- constructor ---------------------------------------------------

(defun make-bayes-model (prior-alist)
  "Create a Bayes model from PRIOR-ALIST, an alist of (HYPOTHESIS . PRIOR).
Priors are automatically normalised so they sum to 1."
  (let ((total (reduce #'+ prior-alist :key #'cdr)))
    (when (zerop total)
      (error "All priors are zero — cannot normalise."))
    (mapcar (lambda (pair)
              (cons (car pair) (/ (cdr pair) total)))
            prior-alist)))

;;; ---------- update --------------------------------------------------------

(defun update (model evidence likelihood-fn)
  "Return a new model with posteriors computed via Bayes' Theorem.
EVIDENCE is an arbitrary datum passed to LIKELIHOOD-FN.
LIKELIHOOD-FN is a function of two arguments (HYPOTHESIS EVIDENCE)
that returns P(evidence | hypothesis)."
  ;; Compute unnormalised posteriors.
  (let* ((unnormalised
           (mapcar (lambda (pair)
                     (cons (car pair)
                           (* (funcall likelihood-fn (car pair) evidence)
                              (cdr pair))))
                   model))
         (marginal (reduce #'+ unnormalised :key #'cdr)))
    (when (zerop marginal)
      (error "Marginal likelihood is zero — evidence is impossible under all hypotheses."))
    ;; Normalise.
    (mapcar (lambda (pair)
              (cons (car pair) (/ (cdr pair) marginal)))
            unnormalised)))

;;; ---------- accessors -----------------------------------------------------

(defun posterior (model hypothesis &key (test #'eql))
  "Return the posterior probability for HYPOTHESIS in MODEL."
  (let ((entry (assoc hypothesis model :test test)))
    (if entry
        (cdr entry)
        (error "Hypothesis ~S not found in model." hypothesis))))

(defun posteriors (model)
  "Return the full posterior alist."
  model)

(defun maximum-a-posteriori (model)
  "Return (HYPOTHESIS . PROBABILITY) for the most probable hypothesis."
  (reduce (lambda (a b) (if (>= (cdr a) (cdr b)) a b))
          model))
