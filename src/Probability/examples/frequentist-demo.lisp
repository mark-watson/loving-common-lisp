;;;; examples/frequentist-demo.lisp — Frequentist medical-screening example
;;;;
;;;; Re-examines the same screening-test scenario from a purely
;;;; frequentist standpoint:
;;;;
;;;;   1. Simulate a clinical trial (N = 100 000).
;;;;   2. Chi-squared test of independence between test result and disease.
;;;;   3. Wilson confidence interval for positive predictive value (PPV).
;;;;   4. Side-by-side comparison with the Bayesian posterior.
;;;;
;;;; Takeaway: a tiny p-value ("highly significant!") coexists with a
;;;; PPV of only ~1–3 %.  Statistical significance ≠ practical utility.

(in-package #:probability)

;;; ---------- simulation ----------------------------------------------------

(defun simulate-screening (&optional (n 100000))
  "Simulate N individuals screened for the rare disease.
Returns (TP FP TN FN) as multiple values."
  (let ((tp 0) (fp 0) (tn 0) (fn 0))
    (dotimes (_ n)
      (let* ((sick    (< (random 1.0d0) *prevalence*))
             (pos     (< (random 1.0d0)
                         (if sick *sensitivity* *false-positive-rate*))))
        (cond
          ((and sick pos)       (incf tp))
          ((and sick (not pos)) (incf fn))
          ((and (not sick) pos) (incf fp))
          (t                   (incf tn)))))
    (values tp fp tn fn)))

;;; ---------- chi-squared test of independence ------------------------------

(defun run-chi-squared-independence (tp fp tn fn)
  "Run a 2×2 chi-squared test of independence.
Contingency table:
             Disease+   Disease−
  Test+        TP         FP
  Test−        FN         TN"
  (let* ((n     (+ tp fp tn fn))
         (nf    (float n 1.0d0))
         ;; Marginals.
         (r1 (+ tp fp))   ; test-positive row
         (r2 (+ fn tn))   ; test-negative row
         (c1 (+ tp fn))   ; disease-positive column
         (c2 (+ fp tn))   ; disease-negative column
         ;; Expected counts under independence: E_ij = (row_i × col_j) / N
         (e-tp (/ (* r1 c1) nf))
         (e-fp (/ (* r1 c2) nf))
         (e-fn (/ (* r2 c1) nf))
         (e-tn (/ (* r2 c2) nf)))
    (chi-squared-test (list tp fp fn tn)
                      (list e-tp e-fp e-fn e-tn))))

;;; ---------- main demo entry point -----------------------------------------

(defun run-frequentist-demo ()
  "Run the frequentist medical-screening demonstration."
  (format t "~%~%================================================================~%")
  (format t "  FREQUENTIST ANALYSIS: Medical Screening Test~%")
  (format t "================================================================~%")

  ;; 1. Simulate
  (multiple-value-bind (tp fp tn fn) (simulate-screening 100000)
    (let ((n (+ tp fp tn fn)))
      (format t "~%--- 1. Simulated Clinical Trial (N = ~:D) ---~%" n)
      (format t "  True  Positives (TP): ~6D~%" tp)
      (format t "  False Positives (FP): ~6D~%" fp)
      (format t "  True  Negatives (TN): ~6D~%" tn)
      (format t "  False Negatives (FN): ~6D~%" fn)

      ;; 2. Chi-squared test
      (format t "~%--- 2. Chi-Squared Test of Independence ---~%")
      (multiple-value-bind (chi2 df p-val)
          (run-chi-squared-independence tp fp tn fn)
        (format t "  chi-squared = ~,2F   df = ~D   p-value ~A~%"
                chi2 df
                (if (< p-val 1.0d-15) "< 1e-15 (essentially zero)"
                    (format nil "= ~,6E" p-val)))
        (format t "~%  Interpretation: the test result and disease status~%")
        (format t "  are NOT independent (we reject H0).  But this only~%")
        (format t "  means the *association exists* — it says nothing about~%")
        (format t "  how strong it is or what it means for one patient.~%"))

      ;; 3. Confidence interval for PPV
      (let* ((positives (+ tp fp))
             (ppv       (if (zerop positives) 0.0d0
                            (/ (float tp 1.0d0) positives))))
        (format t "~%--- 3. Positive Predictive Value (PPV) ---~%")
        (format t "  PPV = TP / (TP + FP) = ~D / ~D = ~,4F  (~,2F %)~%"
                tp positives ppv (* 100.0d0 ppv))
        (multiple-value-bind (lo hi)
            (confidence-interval-proportion tp positives :confidence 0.95d0)
          (format t "  95% Wilson CI for PPV: [~,4F, ~,4F]  (~,2F% – ~,2F%)~%"
                  lo hi (* 100 lo) (* 100 hi))))

      ;; 4. Z-test: is the positive-test rate significantly > prevalence?
      (let ((positives (+ tp fp)))
        (format t "~%--- 4. Z-Test: Positive Rate vs. Prevalence ---~%")
        (multiple-value-bind (z p-val)
            (z-test-proportion positives n *prevalence*)
          (format t "  Observed positive rate: ~,4F%~%"
                  (* 100.0d0 (/ (float positives 1.0d0) n)))
          (format t "  Hypothesised rate (prevalence): ~,4F%~%"
                  (* 100.0d0 *prevalence*))
          (format t "  z = ~,4F   p-value ~A~%"
                  z (if (< p-val 1.0d-15) "< 1e-15" (format nil "= ~,6E" p-val)))
          (format t "~%  The positive rate far exceeds the disease prevalence~%")
          (format t "  because of the 5% false-positive rate — most positives~%")
          (format t "  are healthy people.~%")))

      ;; 5. Contrast with Bayesian result
      (format t "~%--- 5. Bayesian vs. Frequentist Side-by-Side ---~%")
      (let* ((prior   (make-bayes-model `((:disease . ,*prevalence*)
                                          (:healthy . ,(- 1.0d0 *prevalence*)))))
             (updated (update prior :positive-test #'medical-likelihood))
             (p-disease (cdr (assoc :disease (posteriors updated)))))
        (format t "  Bayesian posterior P(disease | positive test) = ~,4F  (~,2F%)~%"
                p-disease (* 100.0d0 p-disease))
        (let ((ppv (/ (float tp 1.0d0) (+ tp fp))))
          (format t "  Frequentist PPV from simulation             = ~,4F  (~,2F%)~%"
                  ppv (* 100.0d0 ppv))))

      (format t "~%  Both frameworks agree: a positive test on a rare disease~%")
      (format t "  gives only about 2% probability of actual illness.~%")
      (format t "  The chi-squared test's tiny p-value is real but misleading~%")
      (format t "  if taken as evidence that the test is *useful* for diagnosis.~%")
      (format t "~%================================================================~%")
      (format t "  Key lesson: statistical significance (small p-value) and~%")
      (format t "  practical significance (high PPV) are different things.~%")
      (format t "================================================================~%")))
  (values))
