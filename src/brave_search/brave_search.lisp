;; Copyright Mark Watson 2024. All Rights Reserved.  https://markwatson.com
;; Apache 2 license.

(in-package #:brave_search)

(defun websearch (query)
  (let* ((key (uiop:getenv "BRAVE_SEARCH_API_KEY"))
         (command
           (concatenate
            'string
            "curl https://api.search.brave.com/res/v1/web/search?q="
            (drakma:url-encode query :utf-8)
            " -H \"X-Subscription-Token: " key "\""
            " -H \"Content-Type: application/json\""))
         (response
           (uiop:run-program command :output :string)))
    ;;(print response) ;; weird: comment this out, and a runtime error is thrown
    (with-input-from-string
        (s response)
      (let ((results (cdar (cddr (assoc :web (json:decode-json s))))))
	(mapcar (lambda (x)
		  (let ((title (cdr (assoc :title x)))
			(url (cdr (assoc :url x)))
			(description (cdr (assoc :description x))))
		    (list url title description)))
		results)))))

;; Example usage:
;; (brave_search:websearch "Sedona Arizona")
