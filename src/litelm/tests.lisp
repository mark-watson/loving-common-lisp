;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;;
;;; Run with:
;;;   sbcl --no-userinit --non-interactive \
;;;     --eval '(load "~/quicklisp/setup.lisp")' \
;;;     --eval '(asdf:load-asd "litelm.asd")' \
;;;     --eval '(asdf:load-system :litelm)' \
;;;     --load tests.lisp

(defpackage #:litelm-tests
  (:use #:cl))

(in-package #:litelm-tests)

(defvar *failures* 0)

(defmacro check (form)
  `(unless ,form
     (incf *failures*)
     (format t "FAIL: ~S~%" ',form)))

;;; ---- JSON encode ----

(check (string= (litelm:json-encode '(("role" . "user") ("content" . "hi")))
                "{\"role\":\"user\",\"content\":\"hi\"}"))
(check (string= (litelm:json-encode '(("stream" . t) ("think" . :false) ("x" . :null)))
                "{\"stream\":true,\"think\":false,\"x\":null}"))
(check (string= (litelm:json-encode '((:max-tokens . 42) (:temperature . 0.6)))
                "{\"max_tokens\":42,\"temperature\":0.6}"))
(check (string= (litelm:json-encode '(("required" . ("a" "b")) ("empty" . ())))
                "{\"required\":[\"a\",\"b\"],\"empty\":[]}"))
(check (string= (litelm:json-encode (format nil "say \"hi\"~%"))
                "\"say \\\"hi\\\"\\n\""))

;;; ---- JSON decode / roundtrip ----

(let ((decoded (litelm:json-decode
                "{\"a\":1,\"b\":[1,2.5,true,null],\"c\":{\"d\":\"e\"}}")))
  (check (eql (litelm::aget decoded "a") 1))
  (check (equal (litelm::aget decoded "b") '(1 2.5d0 t nil)))
  (check (string= (litelm::aget (litelm::aget decoded "c") "d") "e")))
(check (string= (litelm::aget (litelm:json-decode "{\"u\":\"\\u0041BC\"}") "u")
                "ABC"))

;;; ---- model routing ----

(multiple-value-bind (provider model) (litelm:parse-model "openai/gpt-4o")
  (check (eq (litelm::provider-name provider) :openai))
  (check (string= model "gpt-4o")))
;; model names may themselves contain slashes (fireworks)
(multiple-value-bind (provider model)
    (litelm:parse-model "fireworks-ai/accounts/fireworks/models/deepseek-v4-flash")
  (check (eq (litelm::provider-name provider) :fireworks-ai))
  (check (string= model "accounts/fireworks/models/deepseek-v4-flash")))
(handler-case (progn (litelm:parse-model "bogus/m") (check nil))
  (litelm:litelm-error () (check t)))

;;; ---- message translation ----

(check (equal (litelm:json-encode
               (litelm::translate-messages
                '((:system "Be terse.") (:user "Hi"))))
              "[{\"role\":\"system\",\"content\":\"Be terse.\"},{\"role\":\"user\",\"content\":\"Hi\"}]"))

(check (equal (litelm:json-encode
               (litelm::translate-messages
                '((:assistant nil :tool-calls
                   ((:id "c1" :name get_weather :arguments "{\"location\":\"Paris\"}")))
                  (:tool "sunny" :tool-call-id "c1"))))
              (concatenate 'string
                "[{\"role\":\"assistant\","
                "\"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\","
                "\"function\":{\"name\":\"get_weather\","
                "\"arguments\":\"{\\\"location\\\":\\\"Paris\\\"}\"}}]},"
                "{\"role\":\"tool\",\"content\":\"sunny\",\"tool_call_id\":\"c1\"}]")))

;;; ---- tool translation ----

(let ((json (litelm:json-encode
             (litelm::translate-tools
              '((get_weather "Get the current weather for a location"
                 ((location "string" "City name")
                  (units "string" "Units" :required nil
                         :enum ("celsius" "fahrenheit")))))))))
  (check (equal json
                (concatenate 'string
                  "[{\"type\":\"function\",\"function\":{\"name\":\"get_weather\","
                  "\"description\":\"Get the current weather for a location\","
                  "\"parameters\":{\"type\":\"object\",\"properties\":{"
                  "\"location\":{\"type\":\"string\",\"description\":\"City name\"},"
                  "\"units\":{\"type\":\"string\",\"description\":\"Units\","
                  "\"enum\":[\"celsius\",\"fahrenheit\"]}},"
                  "\"required\":[\"location\"]}}}]"))))

;;; ---- live tests against local Ollama ----

(defun ollama-running-p ()
  (ignore-errors
   (dex:get "http://localhost:11434/v1/models") t))

(defvar *ollama-model* "qwen3-vl:2b")

;;; A real tool: the model calls it by name, we execute the Lisp function.

(defun get-weather (location)
  "Simulated weather lookup tool."
  (format nil "sunny and 22C in ~A" location))

(defparameter *get-weather-tool*
  '((get_weather "Get the current weather for a location"
     ((location "string" "City name")))))

(when (ollama-running-p)
  (format t "~&--- live ollama tests (~A) ---~%" *ollama-model*)

  ;; basic completion
  (let ((resp (litelm:completion (concatenate 'string "ollama/" *ollama-model*)
                                 :messages '((:system "Answer in one word.")
                                             (:user "What is 2+2?"))
                                 :max-tokens 4096)))
    (format t "completion => ~S~%" (litelm:response-content resp))
    (check (stringp (litelm:response-content resp))))

  ;; streaming
  (let ((chunks 0))
    (let ((resp (litelm:completion (concatenate 'string "ollama/" *ollama-model*)
                                   :messages '((:user "Count from 1 to 5."))
                                   :stream (lambda (delta)
                                             (declare (ignore delta))
                                             (incf chunks))
                                   :max-tokens 4096)))
      (format t "streamed ~D chunks, content => ~S~%"
              chunks (litelm:response-content resp))
      (check (plusp chunks))
      (check (stringp (litelm:response-content resp)))))

  ;; tool calling: expect a tool call back, execute the real Lisp function,
  ;; then send its result back for a final natural-language response
  (let ((tools *get-weather-tool*))
    (let ((resp (litelm:completion (concatenate 'string "ollama/" *ollama-model*)
                                   :messages '((:user "What is the weather in Paris?"))
                                   :tools tools
                                   :max-tokens 4096)))
      (format t "tool-calls => ~S~%" (litelm:response-tool-calls resp))
      (when (litelm:response-tool-calls resp)
        (let* ((call (first (litelm:response-tool-calls resp)))
               (resp2 (litelm:completion
                       (concatenate 'string "ollama/" *ollama-model*)
                       :messages `((:user "What is the weather in Paris?")
                                   (:assistant nil :tool-calls
                                    ((:id ,(getf call :id)
                                      :name ,(getf call :name)
                                      :arguments ,(litelm:json-encode
                                                   (mapcar (lambda (p)
                                                             (cons (string (car p)) (cdr p)))
                                                           (getf call :arguments))))))
                                   (:tool ,(get-weather
                                            (cdr (assoc :location
                                                        (getf call :arguments))))
                                    :tool-call-id ,(getf call :id)))
                       :tools tools
                       :max-tokens 4096)))
          (format t "follow-up => ~S~%" (litelm:response-content resp2))
          (check (stringp (litelm:response-content resp2))))))))

;;; ---- summary ----

(format t "~&~A failure(s).~%" *failures*)
(finish-output)
(uiop:quit (if (zerop *failures*) 0 1))
