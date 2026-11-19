;;;; basics.lisp -- condition system fundamentals with a small example:
;;;; parsing ages typed by people, some of whom make mistakes.

(in-package #:resilient-llm)

(define-condition bad-age (error)
  ((text :initarg :text :reader bad-age-text))
  (:report (lambda (condition stream)
             (format stream "Not a valid age: ~S" (bad-age-text condition)))))

(defun parse-age (text)
  "Return TEXT as an integer age from 0 to 150. On bad input, signal BAD-AGE
with two restarts: USE-VALUE supplies a replacement age, and SKIP-ENTRY
returns NIL."
  (restart-case
      (let ((age (ignore-errors (parse-integer text))))
        (if (and age (<= 0 age 150))
            age
            (error 'bad-age :text text)))
    (use-value (value)
      :report "Supply an age to use instead."
      :interactive (lambda ()
                     (format *query-io* "Age to use: ")
                     (finish-output *query-io*)
                     (list (read *query-io*)))
      value)
    (skip-entry ()
      :report "Skip this entry."
      nil)))

(defun parse-ages (texts)
  "Parse each string in TEXTS. Entries skipped with SKIP-ENTRY are dropped."
  (loop for text in texts
        for age = (parse-age text)
        when age collect age))

;;; Three policies for the same low-level code. PARSE-AGE never decides
;;; what to do about bad input; the caller chooses by picking a restart.

(defun parse-ages-skipping (texts)
  (handler-bind ((bad-age (lambda (condition)
                            (declare (ignore condition))
                            (invoke-restart 'skip-entry))))
    (parse-ages texts)))

(defun parse-ages-with-default (texts default)
  (handler-bind ((bad-age (lambda (condition)
                            (use-value default condition))))
    (parse-ages texts)))

(defun parse-ages-logging (texts)
  "Log each bad entry, then skip it. The first handler returns normally,
which declines the condition, so the next handler out gets its turn."
  (handler-bind ((bad-age (lambda (condition)
                            (declare (ignore condition))
                            (invoke-restart 'skip-entry))))
    (handler-bind ((bad-age (lambda (condition)
                              (format t "~&Log: ~A~%" condition))))
      (parse-ages texts))))
