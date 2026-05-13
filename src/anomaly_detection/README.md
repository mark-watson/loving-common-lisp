# Anomaly Detection

Gaussian-based anomaly detection for Common Lisp, ported from the Java implementation in `../Java-AI-Book/source-code/anomaly_detection`.

## Overview

This library implements a statistical anomaly detector that:

1. Fits per-feature Gaussian distributions (mean + variance) to training data
2. Tunes an epsilon threshold via cross-validation to minimise classification errors
3. Flags data points whose average Gaussian PDF probability falls below the threshold

The example uses the **Wisconsin Diagnostic Breast Cancer** dataset (647 examples, 9 features + 1 target label).

## Prerequisites

- SBCL (tested with 2.4+)

## Usage

### Run the Wisconsin Example

```bash
cd src/anomaly_detection
sbcl --noinform --load test.lisp
```

### REPL Usage

```lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(asdf:load-system "anomaly-detection")

;; Run the full Wisconsin pipeline:
(anomaly-detection:run-wisconsin)

;; Or build and train on your own data:
;; data = vector of vectors, last column is target
(let ((det (anomaly-detection:build-detector data 10)))
  (anomaly-detection:train det)
  ;; Check if a sample is anomalous:
  (anomaly-detection:anomaly-p det some-vector))
```

## API

### `(build-detector examples num-features)`
Creates a detector from a vector of example vectors. Splits data into training (60%), cross-validation, and test sets.

### `(train det)`
Sweeps 200 epsilon values, picks the best via cross-validation, and evaluates on the test set. Prints precision, recall, and F1. Returns the trained detector.

### `(anomaly-p det x)`
Returns `T` if feature-vector `x` is classified as an anomaly.

### `(mu-values det)` / `(sigma-squared-values det)` / `(best-epsilon det)`
Accessors for model parameters.

## Project Structure

- **`anomaly-detection.lisp`** — Core Gaussian anomaly detection engine
- **`wisconsin.lisp`** — Wisconsin breast cancer dataset loader and driver
- **`test.lisp`** — Test script
- **`data/`** — Contains the cleaned Wisconsin cancer CSV

## License

Apache 2.0. Copyright 2001-2026 Mark Watson.
