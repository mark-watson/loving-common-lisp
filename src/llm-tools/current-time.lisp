;;;; current-time.lisp
;;;;
;;;; A simple command-line tool that returns the current date and time.
;;;; Intended for use as an LLM tool in LM Studio, gemini-clj, and Claude Code.
;;;;
;;;; Usage: ./bin/current-time
;;;; Output: ISO 8601 formatted current date and time

(defpackage #:current-time
  (:use #:cl)
  (:export #:main #:get-current-time))

(in-package #:current-time)

(defun get-current-time ()
  "Return the current local date and time as a formatted string (ISO 8601)."
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore day-of-week dst-p))
    ;; tz is offset west of UTC in hours; convert to +/- east of UTC
    (let* ((tz-sign (if (minusp tz) #\+ #\-))
           (tz-abs (abs tz))
           (tz-hours (floor tz-abs))
           (tz-minutes (round (* (- tz-abs tz-hours) 60))))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~C~2,'0D:~2,'0D"
              year month day
              hour minute second
              tz-sign tz-hours tz-minutes))))

(defun main ()
  "Entry point: print the current time to stdout and exit."
  (format t "~A~%" (get-current-time))
  (sb-ext:exit :code 0))
