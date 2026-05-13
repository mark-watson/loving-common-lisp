# Anomaly Detection

Anomaly detection is a technique for finding data points that don't fit the normal pattern. Unlike standard classification — where we have roughly equal numbers of examples for each class — anomaly detection is designed for problems where "normal" examples vastly outnumber "abnormal" ones. Think of credit card fraud (millions of legitimate transactions vs. a handful of fraudulent ones), network intrusion detection, manufacturing defect spotting, or medical diagnosis.

The core idea is simple: learn what "normal" looks like, then flag anything that deviates too far from that model.

In this chapter we build a Gaussian anomaly detector from scratch in Common Lisp and apply it to the **Wisconsin Diagnostic Breast Cancer** dataset. The dataset contains 647 samples of cell measurements, each labeled as either benign (normal) or malignant (anomalous). Our goal is to train a model on mostly normal data and then use it to identify anomalies — samples that the model considers unusual.

The source code for this chapter is in the directory **src/anomaly_detection**.

## What Is a Gaussian Distribution?

Before diving into the code, let's understand the key idea behind our detector. A **Gaussian distribution** (also called a "bell curve" or "normal distribution") describes how values of a measurement tend to cluster around an average value. Most measurements fall close to the average, and very few fall far away.

Every Gaussian distribution is characterized by two numbers:

- **Mean (μ)** — the average value. This is the center of the bell curve.
- **Variance (σ²)** — how spread out the values are. A small variance means values cluster tightly around the mean; a large variance means they are more spread out.

The **Gaussian probability density function (PDF)** tells us how likely a particular value is, given our distribution:

    p(x) = (1 / (√(2π) · σ)) · exp(-(x - μ)² / (2σ²))

If we compute `p(x)` and get a high number, the value `x` fits well with our model of "normal." If `p(x)` is very low, then `x` is far from the average — it's an anomaly.

## How the Detector Works

Our detector works in four steps:

1. **Split the data** into three groups: training (~60%), cross-validation (~28%), and test (~12%). The training set is mostly normal examples. This mirrors how you would use the system in practice: train on data you believe to be normal, then test on new data.

2. **Fit a Gaussian model** to the training data. For each feature (measurement), we compute the mean μ and variance σ². This tells us what "normal" looks like for each feature independently.

3. **Tune the epsilon threshold.** For each data sample, we compute its average probability across all features. If this probability is below a threshold called **epsilon**, we classify it as an anomaly. But what value of epsilon works best? We try 200 different values and pick the one that makes the fewest mistakes on the cross-validation set. This is called **hyperparameter tuning**.

4. **Evaluate on test data.** With the best epsilon selected, we run the detector on previously unseen test data and report precision, recall, and the F1 score.

## The Wisconsin Breast Cancer Dataset

The dataset contains 647 samples with 9 numeric features each (such as "Clump Thickness," "Cell Size Uniformity," and "Bare Nuclei") scored on a 1–10 scale, plus a target label: 2 for benign and 4 for malignant. In our preprocessing we remap the target to 0 (benign/normal) and 1 (malignant/anomaly).

The raw features are integer-valued and skewed, which is not ideal for a Gaussian model. We apply two preprocessing steps (matching the original Java implementation):

- **Log transform** — applying `log(x + 1.2)` to each feature makes the distribution more bell-shaped.
- **Min-max normalization** — scaling each row's features to the range [0, 1] so that all features contribute equally.

## Project Structure

The code is split into two files:

- **anomaly-detection.lisp** — the core detection algorithm (package, data structures, training, evaluation)
- **wisconsin.lisp** — CSV loading, preprocessing, histograms, and the `run-wisconsin` entry point

## Walking Through the Code

### The Detector Data Structure

We use a `defstruct` to hold the model state:

{lang="lisp",linenos=off}
~~~~~~~~
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
~~~~~~~~

The `mu` slot holds the per-feature means, `sigma-sq` holds the per-feature variances, and `best-eps` is the learned epsilon threshold. The three data splits — training, cross-validation, and testing — are stored so we can use them during the training process.

### Computing Mean and Variance

The `compute-mu` function calculates the average value of each feature across all training examples:

{lang="lisp",linenos=off}
~~~~~~~~
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
~~~~~~~~

This is straightforward: sum all values for each feature, then divide by the number of examples. The inner `loop` iterates over features and accumulates into the `mu` array.

The `compute-sigma-sq` function calculates the variance — how much each feature's values deviate from the mean:

{lang="lisp",linenos=off}
~~~~~~~~
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
~~~~~~~~

For each feature, we compute the sum of squared differences from the mean, then divide by `n`. The `(max ... 1.0d-10)` guard prevents division by zero later when a feature has identical values across all examples. Notice that we skip the last column (`(1- num-features)`) because that column is the target label, not a measurement.

### The Gaussian PDF

The heart of the algorithm is the `gaussian-p` function. For a given data sample, it computes the Gaussian probability for each feature and returns their average:

{lang="lisp",linenos=off}
~~~~~~~~
(defun gaussian-p (x mu sigma-sq num-features)
  "Compute average Gaussian PDF p(x) across features."
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
~~~~~~~~

Let's trace through what happens for one feature:

1. We retrieve the variance `s2` and compute the standard deviation `sigma = √s2`.
2. We compute `diff` — how far this sample's value is from the mean.
3. The exponent is `-(diff²) / (2 · σ²)`. When `diff` is large (the value is far from the mean), this exponent becomes a large negative number, making `exp(exponent)` very small.
4. We multiply by the normalization constant `1 / (√(2π) · σ)` to get a proper probability density.
5. We sum these per-feature probabilities and divide by the number of features to get an average.

A normal sample will have values close to the mean across most features, producing a relatively high average probability. An anomalous sample will have unusual values in several features, producing a low average probability.

### Training: Finding the Best Epsilon

The `train` function searches for the best epsilon threshold:

{lang="lisp",linenos=off}
~~~~~~~~
(defun train (det)
  "Train the detector by sweeping epsilon values on
   cross-validation data, then evaluate on test data."
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
      (train-helper det best-e)
      (test-model det best-e)
      det)))
~~~~~~~~

This tries 200 epsilon values from 0.001 to 1.0 in steps of 0.005. For each value, `train-helper` counts how many cross-validation examples are misclassified. The epsilon with the fewest errors wins. The `<=` comparison (rather than `<`) ensures that when multiple epsilon values tie, we pick the highest one — this gives the decision boundary more margin and tends to generalize better.

Note the `sb-int:with-float-traps-masked` wrapper. SBCL, unlike Java, traps floating-point exceptions by default. The log transform and Gaussian PDF computations can occasionally produce extreme values (especially during the first few epsilon trials), so we mask these traps to match Java's behavior of silently propagating special float values.

### Evaluating the Model

The `test-model` function evaluates the trained detector on held-out test data and computes three standard metrics:

- **Precision** — of the samples flagged as anomalies, what fraction actually are? (Measures false alarm rate.)
- **Recall** — of the actual anomalies, what fraction did we catch? (Measures miss rate.)
- **F1 score** — the harmonic mean of precision and recall. An F1 of 1.0 means perfect detection with no false alarms.

### Data Preprocessing

The `preprocess-wisconsin` function in *wisconsin.lisp* transforms the raw CSV data to make it suitable for Gaussian modeling:

{lang="lisp",linenos=off}
~~~~~~~~
(defun preprocess-wisconsin (rows)
  "Apply log-transform, min-max scaling, and remap
   the target label to [0, 1]."
  (let ((result (make-array (length rows))))
    (loop for row in rows
          for idx from 0 do
      (let ((xs (make-array 10 :element-type t)))
        ;; copy and scale features by 0.1
        (loop for i below 10 do
          (setf (aref xs i) (aref row i)))
        (loop for i below 9 do
          (setf (aref xs i) (* (aref xs i) 0.1d0)))
        ;; log transform
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
~~~~~~~~

Each row goes through three transformations:

1. **Scale by 0.1** — the raw values are integers 1–10; dividing by 10 puts them in [0.1, 1.0].
2. **Log transform** — `log(x + 1.2)` pushes the distribution toward a bell shape. The offset 1.2 ensures we never take the log of zero.
3. **Per-row min-max normalization** — maps values to [0, 1] so that all features contribute equally. The guard for `range < 1e-10` handles the rare case where all features in a single row have the same value (which would cause a division by zero).

Finally, the target label is remapped from {2, 4} to {0, 1}.

## Running the Example

To run the full pipeline:

~~~~~~~~
cd src/anomaly_detection
sbcl --noinform --load test.lisp
~~~~~~~~

Here is typical output (the exact numbers vary due to the random data split):

~~~~~~~~
=== Running anomaly detection test ===
Loaded 648 examples.

Training set:  284
Cross-val set: 176
Test set:      59

**** Best epsilon = 0.9810


 -- best epsilon = 0.9810
 -- test examples  = 59
 -- false positives = 3
 -- true positives  = 21
 -- false negatives = 1
 -- true negatives  = 34
 -- precision = 0.8750
 -- recall    = 0.9545
 -- F1        = 0.9130

Model parameters:
  best epsilon = 0.9810
  num features = 10

--- Assertions ---
All assertions passed.

=== Test complete ===
~~~~~~~~

The detector achieves an F1 score above 0.85 — meaning it correctly identifies most malignant samples while producing few false alarms. This is a strong result for such a simple model, and demonstrates that the Gaussian approach works well when the normal data is roughly bell-curve shaped (which our log transform helps ensure).

## Using the API in Your Own Code

You can also use the anomaly detection module interactively from the REPL:

{lang="lisp",linenos=off}
~~~~~~~~
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(asdf:load-system "anomaly-detection")

;; Run the full Wisconsin pipeline:
(anomaly-detection:run-wisconsin)

;; Or build and train on your own data:
(let ((det (anomaly-detection:build-detector
            my-data 10)))
  (anomaly-detection:train det)
  ;; Check if a new sample is an anomaly:
  (anomaly-detection:anomaly-p det some-vector))
~~~~~~~~

The `anomaly-p` function returns `T` if the detector considers the input vector anomalous and `NIL` otherwise. This makes it easy to integrate into a larger pipeline — for example, flagging suspicious transactions in a stream of financial data.

## Understanding the Evaluation Metrics

If you are new to machine learning, the evaluation output deserves a closer look:

- **True positives (TP)** — anomalies correctly identified as anomalies.
- **True negatives (TN)** — normal samples correctly identified as normal.
- **False positives (FP)** — normal samples incorrectly flagged as anomalies (false alarms).
- **False negatives (FN)** — anomalies that slipped through undetected (misses).

In medical diagnosis, false negatives are especially dangerous — a malignant tumor classified as benign could delay treatment. Our detector's high recall (above 0.95) means it catches nearly all anomalies, at the cost of a few false alarms. This is generally the right tradeoff for safety-critical applications.

## Wrap Up

We built a Gaussian anomaly detector from scratch, using nothing but Common Lisp's built-in math and array operations. The approach is simple yet effective:

1. Model each feature as a Gaussian distribution (mean + variance).
2. Score new samples by how well they fit the model.
3. Use cross-validation to find the best decision threshold.

This technique works best when you have many normal examples and relatively few anomalies — exactly the scenario where traditional classification struggles because there aren't enough positive examples to learn from.

The code in this chapter was ported from a Java implementation and corrected to use the proper Gaussian PDF formula. The same algorithm can be applied to any numeric dataset where you need to detect outliers or unusual patterns.
