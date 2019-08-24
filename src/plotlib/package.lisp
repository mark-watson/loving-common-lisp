;;;; package.lisp

(defpackage #:plotlib
  (:use #:cl #:vecto)
  (:export save-png plot-fill-rect plot-frame-rect plot-size-rect
	   plot-line plot-string plot-string-bold pen-width))
