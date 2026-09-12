;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;;
;;; Run with:
;;;   sbcl --no-userinit --non-interactive \
;;;     --eval '(load "~/quicklisp/setup.lisp")' \
;;;     --eval '(asdf:load-asd "search-apis.asd")' \
;;;     --eval '(asdf:load-system :search-apis)' \
;;;     --load tests.lisp
;;;
;;; Offline tests cover JSON, the provider registry, and the response parsers.
;;; Live tests run only when the matching API key environment variable is set.

(defpackage #:search-apis-tests
  (:use #:cl))

(in-package #:search-apis-tests)

(defvar *failures* 0)

(defmacro check (form)
  `(unless ,form
     (incf *failures*)
     (format t "FAIL: ~S~%" ',form)))

;;; ---- JSON round trip ----

(check (string= (search-apis:json-encode '(("role" . "user") ("content" . "hi")))
                "{\"role\":\"user\",\"content\":\"hi\"}"))
(check (string= (search-apis:json-encode '(("max_results" . 5) ("stream" . t)))
                "{\"max_results\":5,\"stream\":true}"))
(let ((decoded (search-apis:json-decode "{\"a\":1,\"b\":[1,2.5,true,null]}")))
  (check (eql (search-apis::aget decoded "a") 1))
  (check (equal (search-apis::aget decoded "b") '(1 2.5d0 t nil))))

;;; ---- provider registry ----

(dolist (name '(:brave :tavily :perplexity))
  (check (eq name (search-apis:search-provider-name
                   (search-apis:find-search-provider name)))))
(handler-case (progn (search-apis:find-search-provider :bogus) (check nil))
  (search-apis:search-error () (check t)))

;;; ---- Brave parsing ----

(let ((resp (search-apis::parse-brave-response
            (search-apis:json-decode
             (concatenate 'string
               "{\"web\":{\"results\":["
               "{\"title\":\"Sedona\",\"url\":\"https://example.com/sedona\","
               "\"description\":\"A city in Arizona\"}]}}"))
            "Sedona")))
  (check (eq :brave (search-apis:search-response-provider resp)))
  (check (string= "Sedona" (search-apis:search-response-query resp)))
  (check (= 1 (length (search-apis:search-response-results resp))))
  (let ((r (first (search-apis:search-response-results resp))))
    (check (string= "Sedona" (search-apis:search-result-title r)))
    (check (string= "https://example.com/sedona" (search-apis:search-result-url r)))
    (check (string= "A city in Arizona" (search-apis:search-result-content r)))))

;;; ---- Tavily parsing ----

(let ((resp (search-apis::parse-tavily-response
            (search-apis:json-decode
             (concatenate 'string
               "{\"query\":\"Sedona\",\"results\":["
               "{\"title\":\"Sedona\",\"url\":\"https://example.com/sedona\","
               "\"content\":\"A city in Arizona\",\"score\":0.98}]}"))
            "Sedona")))
  (check (eq :tavily (search-apis:search-response-provider resp)))
  (let ((r (first (search-apis:search-response-results resp))))
    (check (string= "A city in Arizona" (search-apis:search-result-content r)))
    (check (= 0.98d0 (search-apis:search-result-score r)))))

;;; ---- Perplexity parsing (answer + citations) ----

(let ((resp (search-apis::parse-perplexity-response
            (search-apis:json-decode
             (concatenate 'string
               "{\"choices\":[{\"message\":{\"content\":\"Sedona is in Arizona.\"}}],"
               "\"citations\":[\"https://example.com/a\",\"https://example.com/b\"]}"))
            "Where is Sedona?")))
  (check (string= "Sedona is in Arizona." (search-apis:search-response-answer resp)))
  (check (= 2 (length (search-apis:search-response-results resp))))
  (check (string= "https://example.com/a"
                  (search-apis:search-result-url
                   (first (search-apis:search-response-results resp))))))

;;; ---- live tests (only when the API key is present) ----

(defmacro when-key (var &body body)
  `(let ((key (uiop:getenv ,var)))
     (when (and key (plusp (length key)))
       (format t "~&--- live ~A tests ---~%" ,var)
       ,@body)))

(when-key "BRAVE_SEARCH_API_KEY"
  (let ((resp (search-apis:websearch "Sedona Arizona" :provider :brave :max-results 3)))
    (format t "brave => ~D results~%" (length (search-apis:search-response-results resp)))
    (check (search-apis:search-response-results resp))))

(when-key "TAVILY_API_KEY"
  (let ((resp (search-apis:websearch "Sedona Arizona" :provider :tavily :max-results 3)))
    (format t "tavily => ~D results~%" (length (search-apis:search-response-results resp)))
    (check (search-apis:search-response-results resp))))

(when-key "PERPLEXITY_API_KEY"
  (let ((resp (search-apis:websearch "Where is Sedona Arizona?" :provider :perplexity)))
    (format t "perplexity => ~S~%" (search-apis:search-response-answer resp))
    (check (stringp (search-apis:search-response-answer resp)))))

;;; ---- summary ----

(format t "~&~A failure(s).~%" *failures*)
(finish-output)
(uiop:quit (if (zerop *failures*) 0 1))
