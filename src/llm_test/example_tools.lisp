(ql:quickload :llm)

(defpackage #:example-tools
  (:use #:cl #:simple-tools))

(in-package #:example-tools)

(define-tool get-weather
    ((location string "The city and state, e.g. San Francisco, CA")
     (unit string "The unit of temperature, e.g. 'c' or 'f'"))
    "Get the current weather in a given location"
  (format t "~%[TOOL EXECUTION] Getting weather for ~A in ~A~%" location unit)
  (if (equal unit "c")
      "22"
      "72"))

(define-tool add-numbers ((a number) (b number))
  "Add two numbers together"
  (format nil "The sum of ~A and ~A is ~A" a b (+ a b)))

(define-tool get-current-time ()
  "Get the current time"
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "Current time: ~2,'0D:~2,'0D:~2,'0D on ~2,'0D/~2,'0D/~A"
            hour min sec month date year)))

(define-tool capitalize-text ((text string))
  "Convert text to uppercase"
  (format nil "Capitalized: ~A" (string-upcase text)))
