;; Test script for anomaly-detection.
;; Run with:
;;   sbcl --noinform --load test.lisp

(require :asdf)
(push (truename "./") asdf:*central-registry*)

(handler-case
    (asdf:load-system "anomaly-detection")
  (error (c)
    (format t "Error loading anomaly-detection: ~A~%" c)
    (uiop:quit 1)))

(format t "~%=== Running anomaly detection test ===~%")

(let ((det (anomaly-detection:run-wisconsin
            :print-histograms nil)))
  (format t "~%--- Assertions ---~%")
  ;; detector should be non-nil
  (assert det)
  ;; epsilon should be positive
  (assert (> (anomaly-detection:best-epsilon det)
             0.0d0))
  ;; mu and sigma-sq vectors should have 10 elements
  (assert (= (length
              (anomaly-detection:mu-values det)) 10))
  (assert (= (length
              (anomaly-detection:sigma-squared-values
               det))
             10))
  (format t "All assertions passed.~%"))

(format t "~%=== Test complete ===~%")
(uiop:quit 0)
