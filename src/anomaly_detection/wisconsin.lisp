;; Copyright Mark Watson 2001-2026. All Rights Reserved.
;; License: Apache 2.0
;;
;; Wisconsin Breast Cancer anomaly detection example.
;; Loads the cleaned CSV, preprocesses (log-transform
;; + min-max scaling), builds and trains a Gaussian
;; anomaly detector, and prints evaluation metrics.

(in-package #:anomaly-detection)

;;; ---- CSV loading & preprocessing ----

(defun load-csv (path)
  "Load a CSV file into a list of double-float vectors."
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (> (length (string-trim '(#\Space
                                         #\Return)
                                       line))
                  0)
          collect
          (let* ((parts (split-csv-line line))
                 (vec (make-array (length parts)
                                  :element-type t)))
            (loop for i below (length parts) do
              (setf (aref vec i)
                    (coerce (read-from-string
                             (nth i parts))
                            'double-float)))
            vec))))

(defun split-csv-line (line)
  "Split LINE by commas. Returns a list of strings."
  (let ((result nil) (start 0))
    (loop for i from 0 below (length line) do
      (when (char= (char line i) #\,)
        (push (subseq line start i) result)
        (setf start (1+ i))))
    (push (string-trim '(#\Return #\Newline)
                       (subseq line start))
          result)
    (nreverse result)))

(defun preprocess-wisconsin (rows)
  "Apply log-transform, min-max scaling, and remap
   the target label to [0, 1].
   Matches the Java WisconsinAnomalyDetection.java."
  (let ((result (make-array (length rows))))
    (loop for row in rows
          for idx from 0 do
      (let ((xs (make-array 10 :element-type t)))
        ;; copy and scale features by 0.1
        (loop for i below 10 do
          (setf (aref xs i) (aref row i)))
        (loop for i below 9 do
          (setf (aref xs i) (* (aref xs i) 0.1d0)))
        ;; log transform to make distributions more
        ;; Gaussian shaped
        (let ((mn 1.0d6) (mx -1.0d6))
          (loop for i below 9 do
            (setf (aref xs i)
                  (log (+ (aref xs i) 1.2d0)))
            (when (< (aref xs i) mn)
              (setf mn (aref xs i)))
            (when (> (aref xs i) mx)
              (setf mx (aref xs i))))
          ;; min-max normalise to [0, 1]
          (let ((range (- mx mn)))
            (if (< range 1.0d-10)
                (loop for i below 9 do
                  (setf (aref xs i) 0.5d0))
                (loop for i below 9 do
                  (setf (aref xs i)
                        (/ (- (aref xs i) mn)
                           range))))))
        ;; remap target: [2, 4] -> [0, 1]
        (setf (aref xs 9)
              (* (- (aref xs 9) 2.0d0) 0.5d0))
        (setf (aref result idx) xs)))
    result))

;;; ---- histogram ----

(defun print-histogram (title data feature-idx
                        min-val max-val num-bins)
  "Print a simple text histogram for FEATURE-IDX."
  (let ((bins (make-array num-bins
                          :initial-element 0)))
    (loop for ex across data do
      (let* ((x (aref ex feature-idx))
             (idx (min (1- num-bins)
                       (floor (* 0.99d0
                                 (- x min-val)
                                 num-bins)
                              max-val))))
        (incf (aref bins idx))))
    (format t "~%~A~%" title)
    (loop for i below num-bins do
      (format t "  ~D~C~D~%" i #\Tab (aref bins i)))))

;;; ---- main entry point ----

(defun run-wisconsin (&key (data-path nil)
                           (print-histograms t)
                           (num-bins 5))
  "Load, preprocess, train, and evaluate anomaly
   detection on the Wisconsin breast cancer dataset."
  (sb-int:with-float-traps-masked
      (:invalid :overflow :divide-by-zero)
    (let* ((path (or data-path
                     (merge-pathnames
                      "data/cleaned_wisconsin_cancer_data.csv"
                      (asdf:system-source-directory
                       "anomaly-detection"))))
           (raw  (load-csv path))
           (data (preprocess-wisconsin raw)))
      (format t "Loaded ~D examples.~%" (length data))
      (when print-histograms
        (let ((names '("Clump Thickness"
                       "Uniformity of Cell Size"
                       "Uniformity of Cell Shape"
                       "Marginal Adhesion"
                       "Single Epithelial Cell Size"
                       "Bare Nuclei"
                       "Bland Chromatin"
                       "Normal Nucleoli"
                       "Mitoses")))
          (loop for name in names
                for i from 0 do
            (print-histogram name data i
                             0.0d0 1.0d0 num-bins))))
      (let ((det (build-detector data 10)))
        (format t "~%Training set:  ~D~%"
                (length (detector-training det)))
        (format t "Cross-val set: ~D~%"
                (length (detector-cross-validation det)))
        (format t "Test set:      ~D~%"
                (length (detector-testing det)))
        (train det)
        ;; print model params
        (format t "~%Model parameters:~%")
        (format t "  best epsilon = ~,4F~%"
                (best-epsilon det))
        (format t "  num features = ~D~%"
                (detector-num-features det))
        det))))
