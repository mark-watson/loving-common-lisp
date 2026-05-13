;; Copyright Mark Watson 2001-2026. All Rights Reserved.
;; License: Apache 2.0

(defpackage #:anomaly-detection
  (:use #:cl)
  (:export #:build-detector
           #:train
           #:anomaly-p
           #:mu-values
           #:sigma-squared-values
           #:best-epsilon
           #:run-wisconsin))

(in-package #:anomaly-detection)

;;; ---- constants ----

(defconstant +sqrt-2-pi+ 2.50662827463d0
  "Precomputed sqrt(2*pi).")

;;; ---- data structures ----

(defstruct detector
  "Gaussian anomaly detector.
   Fits per-feature mean and variance, then tunes an
   epsilon threshold via cross-validation."
  (num-features     0   :type fixnum)
  (mu               #() :type simple-vector)
  (sigma-sq         #() :type simple-vector)
  (best-eps         0.02d0 :type double-float)
  (training         #() :type simple-vector)
  (cross-validation #() :type simple-vector)
  (testing          #() :type simple-vector))

;;; ---- helpers ----

(defun split-data (examples num-features)
  "Split EXAMPLES into training / cross-validation / test.
   Training gets ~60 %% of rows, preferring normal (target
   < 0.5) rows but allowing ~10 %% anomalies through.
   Returns (values training cv test)."
  (let ((train-list nil)
        (cv-list    nil)
        (test-list  nil)
        (outcome-idx (1- num-features)))
    (loop for ex across examples do
      (cond
        ((< (random 1.0d0) 0.6d0)
         ;; Keep normal examples; allow ~10% anomalies
         (when (or (< (aref ex outcome-idx) 0.5d0)
                   (< (random 1.0d0) 0.1d0))
           (push ex train-list)))
        ((< (random 1.0d0) 0.7d0)
         (push ex cv-list))
        (t
         (push ex test-list))))
    (values (coerce (nreverse train-list)
                    'simple-vector)
            (coerce (nreverse cv-list)
                    'simple-vector)
            (coerce (nreverse test-list)
                    'simple-vector))))

(defun compute-mu (examples num-features)
  "Compute per-feature mean over EXAMPLES."
  (let ((n (length examples))
        (mu (make-array num-features
                        :initial-element 0.0d0)))
    (when (zerop n)
      (return-from compute-mu mu))
    (loop for ex across examples do
      (loop for f below num-features do
        (incf (aref mu f) (aref ex f))))
    (loop for f below num-features do
      (setf (aref mu f) (/ (aref mu f) n)))
    mu))

(defun compute-sigma-sq (examples mu num-features)
  "Compute per-feature variance over EXAMPLES.
   Skips the last column (target label)."
  (let ((n (length examples))
        (sigma-sq (make-array num-features
                              :initial-element 0.0d0)))
    (loop for f below (1- num-features) do
      (let ((sum 0.0d0))
        (loop for ex across examples do
          (let ((diff (- (aref ex f) (aref mu f))))
            (incf sum (* diff diff))))
        (setf (aref sigma-sq f)
              (max (/ sum n) 1.0d-10))))
    sigma-sq))

(defun gaussian-p (x mu sigma-sq num-features)
  "Compute average Gaussian PDF p(x) across features.
   p(x_i) = (1/(sqrt(2*pi)*sigma))
             * exp(-(x_i - mu_i)^2 / (2*sigma_i^2))
   Returns the mean across all feature dimensions."
  (let ((sum 0.0d0))
    (loop for f below (1- num-features) do
      (let* ((s2 (aref sigma-sq f))
             (sigma (sqrt s2))
             (diff (- (aref x f) (aref mu f)))
             (exponent (/ (- (* diff diff))
                          (* 2.0d0 s2))))
        (incf sum (* (/ 1.0d0 (* +sqrt-2-pi+ sigma))
                     (exp exponent)))))
    (/ sum num-features)))

;;; ---- public API ----

(defun build-detector (examples num-features)
  "Build a detector from EXAMPLES (a vector of vectors).
   The last column of each example is the target label
   (0 = normal, 1 = anomaly)."
  (multiple-value-bind (train cv test)
      (split-data examples num-features)
    (let ((mu (compute-mu train num-features)))
      (make-detector
       :num-features     num-features
       :mu               mu
       :sigma-sq         (make-array num-features
                                     :initial-element
                                     0.0d0)
       :training         train
       :cross-validation cv
       :testing          test))))

(defun train-helper (det epsilon)
  "One training pass: compute sigma-sq from training
   data, then count cross-validation errors."
  (let* ((nf    (detector-num-features det))
         (train (detector-training det))
         (mu    (detector-mu det))
         (cv    (detector-cross-validation det))
         (s2    (compute-sigma-sq train mu nf)))
    (setf (detector-sigma-sq det) s2)
    (let ((errors 0))
      (loop for x across cv do
        (let ((prob (gaussian-p x mu s2 nf))
              (target (aref x (1- nf))))
          (cond
            ;; target > 0.5 => anomaly
            ((> target 0.5d0)
             (when (> prob epsilon) (incf errors)))
            ;; target <= 0.5 => normal
            (t
             (when (< prob epsilon)
               (incf errors))))))
      errors)))

(defun test-model (det epsilon)
  "Evaluate the model on held-out test data.
   Returns (values precision recall f1)."
  (let ((nf  (detector-num-features det))
        (mu  (detector-mu det))
        (s2  (detector-sigma-sq det))
        (tp 0) (fp 0) (fn 0) (tn 0))
    (loop for x across (detector-testing det) do
      (let ((prob (gaussian-p x mu s2 nf))
            (target (aref x (1- nf))))
        (cond
          ((> target 0.5d0)           ; actual anomaly
           (if (> prob epsilon)
               (incf fn) (incf tp)))
          (t                          ; actual normal
           (if (< prob epsilon)
               (incf fp) (incf tn))))))
    (let* ((precision (if (zerop (+ tp fp)) 0.0d0
                          (/ (float tp 1.0d0)
                             (+ tp fp))))
           (recall    (if (zerop (+ tp fn)) 0.0d0
                          (/ (float tp 1.0d0)
                             (+ tp fn))))
           (f1        (if (zerop (+ precision recall))
                          0.0d0
                          (/ (* 2.0d0 precision recall)
                             (+ precision recall)))))
      (format t "~%~% -- best epsilon = ~,4F~%" epsilon)
      (format t " -- test examples  = ~D~%"
              (length (detector-testing det)))
      (format t " -- false positives = ~D~%" fp)
      (format t " -- true positives  = ~D~%" tp)
      (format t " -- false negatives = ~D~%" fn)
      (format t " -- true negatives  = ~D~%" tn)
      (format t " -- precision = ~,4F~%" precision)
      (format t " -- recall    = ~,4F~%" recall)
      (format t " -- F1        = ~,4F~%" f1)
      (values precision recall f1))))

(defun train (det)
  "Train the detector by sweeping epsilon values on
   cross-validation data, then evaluate on test data.
   Returns the detector (mutated in place)."
  (sb-int:with-float-traps-masked
      (:invalid :overflow :divide-by-zero)
    (let ((best-err  1d10)
          (best-e    0.001d0))
      (loop for i below 200
            for eps = (+ 0.001d0 (* 0.005d0 i))
            for err = (train-helper det eps)
            do (when (<= err best-err)
                 (setf best-err err
                       best-e eps)))
      (format t "~%**** Best epsilon = ~,4F~%"
              best-e)
      (setf (detector-best-eps det) best-e)
      ;; retrain with best epsilon to set sigma-sq
      (train-helper det best-e)
      ;; evaluate on test set
      (test-model det best-e)
      det)))

(defun anomaly-p (det x)
  "Return T if feature-vector X is an anomaly."
  (< (gaussian-p x
                  (detector-mu det)
                  (detector-sigma-sq det)
                  (detector-num-features det))
     (detector-best-eps det)))

(defun mu-values (det)
  "Return the per-feature mean vector."
  (detector-mu det))

(defun sigma-squared-values (det)
  "Return the per-feature variance vector."
  (detector-sigma-sq det))

(defun best-epsilon (det)
  "Return the tuned epsilon threshold."
  (detector-best-eps det))
