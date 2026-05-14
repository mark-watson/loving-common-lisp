;;;; correlation.lisp — Correlation helpers
;;;;
;;;; NOTE: Correlation measures *association*, NOT causation.
;;;; A high |r| between X and Y does not imply that X causes Y,
;;;; that Y causes X, or even that they share a direct mechanism.
;;;; Confounders, selection bias, and reverse causation are always
;;;; possible.  Use these statistics as descriptive summaries, not
;;;; as evidence of causal relationships.

(in-package #:probability)

;;; ---------- utilities -----------------------------------------------------

(defun mean (xs)
  "Arithmetic mean of a list of numbers."
  (/ (reduce #'+ xs) (length xs)))

(defun std-dev (xs)
  "Population standard deviation of a list of numbers."
  (let* ((m (mean xs))
         (ss (reduce #'+ xs :key (lambda (x) (expt (- x m) 2)))))
    (sqrt (/ ss (length xs)))))

(defun rank-list (xs)
  "Return a list of ranks (1-based, average ties) for XS."
  (let* ((n (length xs))
         (indexed (loop for x in xs for i from 0 collect (cons x i)))
         (sorted  (sort (copy-list indexed) #'< :key #'car))
         (ranks   (make-array n)))
    ;; Walk through sorted list, assigning average rank to ties.
    (loop with i = 0
          while (< i n)
          do (let ((j i))
               ;; Find the end of the tie group.
               (loop while (and (< j n)
                                (= (car (nth j sorted))
                                   (car (nth i sorted))))
                     do (incf j))
               ;; Average rank for this group (1-based).
               ;; avg of (i+1)..(j) = (i+1+j)/2
               (let ((avg (/ (+ i 1 j) 2.0d0)))
                 (loop for k from i below j
                       do (setf (aref ranks (cdr (nth k sorted))) avg)))
               (setf i j)))
    (coerce ranks 'list)))

;;; ---------- Pearson r -----------------------------------------------------

(defun pearson-r (xs ys)
  "Pearson product-moment correlation coefficient.
XS and YS must be equal-length lists of numbers.

WARNING: measures linear association only — not causation."
  (assert (= (length xs) (length ys)) ()
          "XS and YS must have the same length.")
  (let* ((n   (length xs))
         (mx  (mean xs))
         (my  (mean ys))
         (sx  (std-dev xs))
         (sy  (std-dev ys)))
    (when (or (zerop sx) (zerop sy))
      (return-from pearson-r 0.0d0))   ; constant variable ⇒ no correlation
    (/ (reduce #'+ (mapcar (lambda (x y) (* (- x mx) (- y my))) xs ys))
       (* n sx sy))))

;;; ---------- Spearman ρ ----------------------------------------------------

(defun spearman-rho (xs ys)
  "Spearman rank-order correlation coefficient.
Converts XS and YS to ranks then computes Pearson-r on those ranks.

WARNING: measures monotonic association only — not causation."
  (pearson-r (rank-list xs) (rank-list ys)))

;;; ---------- correlation matrix --------------------------------------------

(defun correlation-matrix (data-alist)
  "Given DATA-ALIST of (NAME . VALUES-LIST) pairs, return
a list of (NAME-A NAME-B . PEARSON-R) triples for all pairs."
  (loop for (name-a . vals-a) in data-alist
        append (loop for (name-b . vals-b) in data-alist
                     collect (cons name-a (cons name-b (pearson-r vals-a vals-b))))))
