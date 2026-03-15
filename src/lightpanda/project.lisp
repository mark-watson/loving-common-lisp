;;;; project.lisp — Package definition for lightpanda-browser
;;;;
;;;; All implementation code lives in lightpanda.lisp (package lightpanda).
;;;; This file exists solely to declare the lightpanda-browser package for
;;;; compatibility with any code that references it.

(defpackage #:lightpanda-browser
  (:use #:cl #:lightpanda))

(in-package #:lightpanda-browser)
