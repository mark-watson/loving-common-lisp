;; Copyright Mark Watson 2020. All Rights Reserved.  https://markwatson.com
;; Apache 2 license.

(in-package #:bing)

(defun get-wikidata-uri (query)
  (let ((sr (websearch (concatenate 'string "site:wikidata.org " query))))
    (cadar sr)))

(defun websearch (query)
  (let* ((key (uiop:getenv "BING_SEARCH_V7_SUBSCRIPTION_KEY"))
         (endpoint (uiop:getenv "BING_SEARCH_V7_ENDPOINT"))
         (command
          (concatenate
	   'string
	   "curl -v -X GET \""  endpoint "?q="
	   (drakma:url-encode query :utf-8)
	   "&mkt=en-US&limit=4\""
	   " -H \"Ocp-Apim-Subscription-Key: " key "\""))
         (response
          (uiop:run-program command :output :string)))
    (print response) ;; weird: comment this out, and a runtime error is thrown
    (with-input-from-string
	(s response)
      (let* ((json-as-list (json:decode-json s))
             (values (cdadr (cddr (nth 2 json-as-list)))))
	(pprint json-as-list)  ; uncomment this to see how following expression works:
	(mapcar #'(lambda (x)
                    (let ((name (assoc :name x))
			  (display-uri (assoc :display-url x))
			  (snippet (assoc :snippet x)))
		      (list (cdr name) (cdr display-uri) (cdr snippet))))
		values)))))
