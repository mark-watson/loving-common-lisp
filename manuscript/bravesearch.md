# Using the Brave Search APIs

*Note: I started using the Brave search APIs in June 2024 and replaced the Microsoft Bing search chapter in previous editions with the following material.*

You will need to get a free API key at [https://brave.com/search/api/](https://brave.com/search/api/) to use the following code examples. You can use the search API 2000 times a month for free or pay $5/month to get 20 million API calls a month.

## Setting an Environment Variable for the Access Key for Brave Search APIs

Once you get a key for [https://brave.com/search/api/](https://brave.com/search/api/) set the following environment variable:

{lang="bash",linenos=off}
~~~~~~~~
export BRAVE_SEARCH_API_KEY=BSGhQ-Nd-......
~~~~~~~~


That is not my real subscription key!


## Example Search Script

Get the code for this example using (change directory to your Quicklist **local-projects** directory):

    cd ~/.roswell/local-projects/brave_search # if you use Roswell in install and update SBCL, etc.
    cd ~/quicklisp/local-projects  # if you DO NOT use Roswell
    git clone https://github.com/mark-watson/brave_search.git
    cd brave_search


It takes very little Common Lisp code to access the Brave search APIs. The function **websearch** makes a generic web search query. I will list the entire library with comments to follow:

{lang="lisp",linenos=on}
~~~~~~~~
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
~~~~~~~~

We get the Brave access key and the search API endpoint in lines 8-9. Lines 10-16 create a complete call to the **curl* command line utility. We spawn a process to run **curl** and capture the string output in the variable **response** in lines 17-18. You might want to add a few print statements to see typical values for the variables **command** and **response**. The response data is JSON data encoded in a string, with straightforward code in lines 19-28 to parse out the values we want.

The following repl listing shows this library in use (most output not shown):

{lang="bash",linenos=off}
~~~~~~~~
 $ sbcl
*  (ql:quickload "brave_search")
To load "brave_search":
  Load 1 ASDF system:
    brave_search
; Loading "brave_search"
..................
("brave_search")
* (brave_search:websearch "Sedona Arizona")
(("https://visitsedona.com/"
  "Visit Sedona | The official site of the Sedona Tourism Bureau"
  "The official site of the <strong>Sedona</strong>, AZ tourism bureau. Find out the best places to stay, eat, and relax in our beautiful central <strong>Arizona</strong> resort town.")
 ("https://www.sedonaaz.gov/" "City of Sedona | Home"
  "The City of <strong>Sedona</strong> wastewater department has experienced ...
~~~~~~~~

