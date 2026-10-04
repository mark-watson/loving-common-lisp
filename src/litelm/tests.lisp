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

;;; ---- oMLX local provider and default model ----

;; the default model is the local oMLX server
(check (string= litelm:*default-model* "omlx/Laguna-XS-2.1-6bit"))
(multiple-value-bind (provider model) (litelm:parse-model litelm:*default-model*)
  (check (eq (litelm::provider-name provider) :omlx))
  (check (string= model "Laguna-XS-2.1-6bit")))
(let ((omlx (litelm:find-provider :omlx)))
  (check (string= (litelm::provider-base-url omlx) "http://localhost:8000/v1"))
  (check (not (litelm::provider-requires-key omlx))))
;; NIL means *DEFAULT-MODEL*, anything else passes through
(check (string= (litelm::resolve-model nil) litelm:*default-model*))
(check (string= (litelm::resolve-model "openai/gpt-4o") "openai/gpt-4o"))
;; a fully qualified MLX repo id also routes to omlx (first slash splits)
(multiple-value-bind (provider model)
    (litelm:parse-model "omlx/mlx-community/Laguna-XS-2.1-6bit")
  (check (eq (litelm::provider-name provider) :omlx))
  (check (string= model "mlx-community/Laguna-XS-2.1-6bit")))

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

(defun ollama-model-ids ()
  "Model ids served by a local Ollama server, or NIL when it is not running."
  (ignore-errors
   (let ((json (litelm:json-decode
                (dex:get "http://localhost:11434/v1/models"
                         :read-timeout 5 :connect-timeout 2))))
     (loop for m in (litelm::aget json "data")
           collect (litelm::aget m "id")))))

(defvar *ollama-model* "qwen3-vl:2b")

;;; A real tool: the model calls it by name, we execute the Lisp function.

(defun get-weather (location)
  "Simulated weather lookup tool."
  (format nil "sunny and 22C in ~A" location))

(defparameter *get-weather-tool*
  '((get_weather "Get the current weather for a location"
     ((location "string" "City name")))))

;; run the live tests only when the server is up and actually serves the model
(when (member *ollama-model* (ollama-model-ids) :test #'string=)
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

;;; ---- live tests against a local oMLX server ----

(defun omlx-model-ids ()
  "Model ids served by a local oMLX server, or NIL when it is not running."
  (ignore-errors
   (let ((json (litelm:json-decode
                (dex:get "http://localhost:8000/v1/models"
                         :read-timeout 5 :connect-timeout 2))))
     (loop for m in (litelm::aget json "data")
           collect (litelm::aget m "id")))))

(defparameter *omlx-models* (omlx-model-ids))

(defparameter *omlx-model*
  (or (first (member "Laguna-XS-2.1-6bit" *omlx-models* :test #'string=))
      (first *omlx-models*)))

(when *omlx-model*
  (format t "~&--- live oMLX tests (~A) ---~%" *omlx-model*)

  ;; the default-model path: MODEL is NIL, so *DEFAULT-MODEL* decides
  (when (member "Laguna-XS-2.1-6bit" *omlx-models* :test #'string=)
    (let ((resp (litelm:completion nil
                                   :messages '((:user "What is 2+2? Answer with just the number."))
                                   :max-tokens 2048)))
      (format t "default-model completion => ~S~%" (litelm:response-content resp))
      (check (stringp (litelm:response-content resp)))))

  ;; explicit "omlx/model" routing
  (let ((resp (litelm:completion (concatenate 'string "omlx/" *omlx-model*)
                                 :messages '((:user "Say the single word: pong"))
                                 :max-tokens 2048)))
    (format t "omlx completion => ~S~%" (litelm:response-content resp))
    (check (stringp (litelm:response-content resp))))

  ;; streaming
  (let ((chunks 0))
    (let ((resp (litelm:completion (concatenate 'string "omlx/" *omlx-model*)
                                   :messages '((:user "Count from 1 to 5."))
                                   :stream (lambda (delta)
                                             (declare (ignore delta))
                                             (incf chunks))
                                   :max-tokens 2048)))
      (format t "omlx streamed ~D chunks, content => ~S~%"
              chunks (litelm:response-content resp))
      (check (plusp chunks))
      (check (stringp (litelm:response-content resp))))))

;;; ---- summary ----

(format t "~&~A failure(s).~%" *failures*)
(finish-output)
(uiop:quit (if (zerop *failures*) 0 1))
