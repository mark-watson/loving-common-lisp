;; Misc. plotting examples using the vecto library

(ql:quickload :vecto) ;; Zach Beane's plotting library
(defpackage #:plotlib
  (:use #:cl #:vecto)
  (:export save-png plot-fill-rect plot-frame-rect plot-size-rect
	   plot-line plot-string plot-string-bold pen-width))

(in-package #:plotlib)

;; the coordinate (0,0) is the lower left corner of the plotting area.
;; Increasing the y coordinate is "up page" and increasing x is "to the right"

;; fills a rectangle with a gray-scale value
(defun plot-fill-rect (x y width height gscale) ; 0 < gscale < 1
  (set-rgb-fill gscale gscale gscale)
  (rectangle x y width height)
  (fill-path))

;; plots a frame rectangle
(defun plot-frame-rect (x y width height)
  (set-line-width 1)
  (set-rgb-fill 1 1 1)
  (rectangle x y width height)
  (stroke))

;; plots a rectangle value pixels wide
(defun plot-size-rect (x y width height num-pixels-wide)
  (plot-frame-rect x y width height)
  (plot-fill-rect x y num-pixels-wide num-pixels-wide 0.5))

(defun plot-line(x1 y1 x2 y2)
  (set-line-width 1)
  (set-rgb-fill 0 0 0)
  (move-to x1 y1)
  (line-to x2 y2)
  (stroke))

(defun plot-string(x y str)
  (let ((font (get-font "OpenSans-Regular.ttf")))
    (set-font font 12)
    (set-rgb-fill 0 0 0)
    (draw-string x y str)))

(defun plot-string-bold(x y str)
  (let ((font (get-font "OpenSans-Bold.ttf")))
    (set-font font 12)
    (set-rgb-fill 0 0 0)
    (draw-string x y str)))


(defun test-plotlib (file)
  (with-canvas (:width 90 :height 90)
    (plot-fill-rect 5 10 15 30 0.2) ; black
    (plot-fill-rect 25 30 30 7 0.7) ; medium gray
    (plot-frame-rect 10 50 30 7)
    (plot-size-rect 24 16 10 10 5)
    (plot-size-rect 39 16 10 10 3)
    (plot-size-rect 54 16 10 10 9)
    (plot-line 90 5 10 5)
    (plot-string 10 65 "test 1 2 3")
    (plot-string-bold 10 78 "Hello")
    (save-png file)))

(defun pen-width (width)
  (set-line-width width))

;;(test-plotlib "test-plotlib.png")
