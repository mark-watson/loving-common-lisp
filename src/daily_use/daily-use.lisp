;;;; daily-use.lisp — Interactive Gemini REPL with search grounding and cache
;;;;
;;;; Ordinary questions go through litelm, the book's provider-neutral LLM
;;;; client.  The "!" prefix asks the gemini library for Google Search
;;;; grounding, because Google Search is a Gemini-native tool that the
;;;; OpenAI-compatible layer litelm speaks does not expose.
;;;;
;;;; Commands:
;;;;   <text>          Ask Gemini a question (plain, no search)
;;;;   !<text>         Ask Gemini with Google Search grounding
;;;;   >               Add last answer to the persistent cache
;;;;   !               Clear cache entries older than one week
;;;;   h / H / help    Show help
;;;;   q / quit / exit  Exit the REPL
;;;;   Ctrl-D          Exit the REPL

(defpackage #:daily-use
  (:use #:cl)
  (:export #:main))

(in-package #:daily-use)

;;; ---- Configuration ----

(defvar *model* "gemini/gemini-3.1-flash-lite"
  "litelm model string, \"provider/model-name\". The prefix selects the
   provider; the remainder is the model id sent to the API.")

(defvar *system-prompt*
  "You are a concise, helpful assistant. Prefer short, direct answers."
  "System message sent with every plain (non-search) question.")

(defvar *cache-db-path*
  (merge-pathnames ".daily-use-cache.db" (user-homedir-pathname)))

(defvar *history-file*
  (merge-pathnames ".daily-use-history" (user-homedir-pathname))
  "Where cl-readline stores the input history between sessions.")

(defvar *whitespace* '(#\Space #\Tab #\Newline #\Return)
  "Characters trimmed from user input. Newline and return belong here: a
   pasted line can end with one, and an untrimmed newline would otherwise be
   sent to the API as part of the question.")

;;; ---- State ----

(defvar *cache* nil
  "The cache-engine instance for persisting useful answers.")
(defvar *last-answer* nil
  "The last answer returned by Gemini, available for caching with '>'.")

;;; ---- Keyword extraction ----

(defvar *stop-words*
  '("a" "an" "the" "is" "are" "was" "were" "be" "been" "being"
    "have" "has" "had" "do" "does" "did" "will" "would" "shall" "should"
    "may" "might" "must" "can" "could" "am" "it" "its"
    "in" "on" "at" "to" "for" "of" "with" "by" "from" "as"
    "and" "or" "but" "not" "no" "nor" "so" "yet"
    "this" "that" "these" "those" "what" "which" "who" "whom"
    "i" "me" "my" "we" "our" "you" "your" "he" "she" "they" "them"
    "how" "when" "where" "why" "if" "then" "than" "about")
  "Common English stop words to filter from search queries.")

(defvar *punctuation*
  '(#\? #\! #\. #\, #\; #\: #\" #\' #\( #\) #\[ #\])
  "Characters stripped from the ends of a word. Note that % and _ are not
   here: they are LIKE metacharacters, and cache-engine escapes them when it
   builds the query, so callers do not have to work around them.")

(defun extract-keywords (text)
  "Extracts meaningful keywords from TEXT by splitting on whitespace,
   downcasing, removing punctuation, and filtering stop words and short words."
  (let* ((downcased (string-downcase text))
         (words (uiop:split-string downcased :separator *whitespace*))
         (cleaned (mapcar (lambda (w) (string-trim *punctuation* w)) words)))
    (remove-if (lambda (w)
                 (or (<= (length w) 2)
                     (member w *stop-words* :test #'string=)))
               cleaned)))

;;; ---- Cache context builder ----

(defun build-context-from-cache (query)
  "Retrieves cached items relevant to QUERY and builds a context string.
   Uses bag-of-words matching: extracts keywords from the query and finds
   cached entries containing any of those keywords."
  (let* ((keywords (extract-keywords query))
         (items (when keywords
                  (cache-engine:lookup *cache* keywords
                                       :limit 10 :match-any t))))
    (if items
        (format nil "Use the following context from previous conversations when answering:~%~%~{- ~A~%~}~%---~%~%"
                items)
        "")))

;;; ---- Query dispatch ----

(defun gemini-model-id ()
  "The bare model id for the native Gemini API — *MODEL* without its
   \"provider/\" prefix, which only litelm needs."
  (let ((slash (position #\/ *model*)))
    (if slash (subseq *model* (1+ slash)) *model*)))

(defun ask-gemini (prompt &key search-p)
  "Sends PROMPT to Gemini, optionally with Google Search grounding, and
   prepends any cached context relevant to it.

   Returns two values: the answer text, and a flag that is true when that
   text is an error message rather than an answer.

   Plain questions go through litelm, which routes on the \"provider/model\"
   prefix and normalises the provider differences. The search-grounded
   variant uses the gemini library instead, because Google Search grounding
   is a Gemini-native tool: the OpenAI-compatible layer does not expose it."
  (let* ((context (build-context-from-cache prompt))
         (full-prompt (concatenate 'string context prompt)))
    (handler-case
        (if search-p
            (values (gemini:generate-with-search full-prompt (gemini-model-id))
                    nil)
            (values (litelm:response-content
                     (litelm:completion *model*
                                        :messages (list (list :system *system-prompt*)
                                                        (list :user full-prompt))
                                        :temperature 0.3
                                        :max-tokens 2048))
                    nil))
      ;; litelm signals a condition per failure mode, so each one gets the
      ;; advice the user actually needs. Order matters: rate-limit-error and
      ;; authentication-error are both subtypes of api-error.
      (litelm:rate-limit-error (c)
        (values (format nil "[Rate limited (HTTP ~A). Wait a moment and ask again.]"
                        (litelm:api-error-status c))
                t))
      (litelm:authentication-error (c)
        (values (format nil "[Authentication failed (HTTP ~A). Check that GEMINI_API_KEY or GOOGLE_API_KEY is valid.]"
                        (litelm:api-error-status c))
                t))
      (litelm:context-window-exceeded-error (c)
        (values (format nil "[Context window exceeded (HTTP ~A). The cached context is too large; try clearing the cache.]"
                        (litelm:api-error-status c))
                t))
      (litelm:api-error (c)
        (values (format nil "[API error (HTTP ~A): ~A]"
                        (litelm:api-error-status c)
                        (litelm:api-error-body c))
                t))
      (litelm:litelm-error (c)
        (values (format nil "[Configuration error: ~A]" c) t))
      ;; Anything else — including network failures from the gemini library —
      ;; must not drop the user into the debugger.
      (error (c)
        (values (format nil "[Error calling the Gemini API: ~A]" c) t)))))

;;; ---- Help text ----

(defun print-help ()
  (format t "~%  Gemini Daily-Use REPL~%")
  (format t "  ─────────────────────────────────────────~%")
  (format t "  <text>         Ask Gemini a question~%")
  (format t "  !<text>        Ask with Google Search grounding~%")
  (format t "  >              Add last answer to cache~%")
  (format t "  !              Clear cache entries older than 1 week~%")
  (format t "  h / help       Show this help~%")
  (format t "  q / quit       Exit~%")
  (format t "  Ctrl-D         Exit~%")
  (format t "  ─────────────────────────────────────────~%")
  (format t "  Model: ~A~%" *model*)
  (format t "  Cache: ~A (~D items)~%~%"
          *cache-db-path* (cache-engine:count-items *cache*)))

;;; ---- Pretty-print answer ----

(defun display-answer (text error-p)
  "Prints the answer with a visual separator. An error message is shown but
   is deliberately not remembered, so '>' can never cache a failure into the
   persistent context."
  (cond
    ((null text)
     (format t "~%  [No response from Gemini — check model name or API key]~%~%"))
    (error-p
     (format t "~%~A~%~%" text))
    (t
     (format t "~%~A~%~%" text)
     (setf *last-answer* text))))

;;; ---- REPL helpers ----

(defun clear-old-cache ()
  "Drop cache entries older than a week and report what happened."
  (let ((before (cache-engine:count-items *cache*)))
    (cache-engine:clear-cache-older-one-week *cache*)
    (let ((after (cache-engine:count-items *cache*)))
      (format t "  [Cleared ~D old entries. ~D items remain]~%"
              (- before after) after))))

(defun load-history ()
  "Load previous input history into readline.
   A missing file is normal on the first run, so it is not an error."
  (handler-case
      (when (probe-file *history-file*)
        (rl:read-history (namestring *history-file*)))
    (error (c)
      (format t "  [Could not read history from ~A: ~A]~%" *history-file* c))))

(defun save-history ()
  "Write the input history back to *HISTORY-FILE* so it survives the session."
  (handler-case
      (rl:write-history (namestring *history-file*))
    (error (c)
      (format t "  [Could not write history to ~A: ~A]~%" *history-file* c))))

;;; ---- REPL ----

(defun repl-loop ()
  "Main REPL loop with cl-readline for line editing and history.

   History is loaded from *HISTORY-FILE* on entry and written back on exit,
   inside an unwind-protect so that an error still saves what you typed."
  (format t "~%  Gemini Daily-Use REPL  (type 'h' for help)~%~%")
  (load-history)
  (unwind-protect
       (loop
         (let ((input (rl:readline :prompt "gemini> " :add-history t)))
           ;; Handle EOF (Ctrl-D)
           (when (null input)
             (format t "~%Goodbye.~%")
             (return))

           (let ((trimmed (string-trim *whitespace* input)))
             (cond
               ;; Empty line — skip
               ((string= trimmed "")
                nil)

               ;; Quit
               ((member trimmed '("q" "quit" "exit") :test #'string-equal)
                (format t "Goodbye.~%")
                (return))

               ;; Help
               ((member trimmed '("h" "help") :test #'string-equal)
                (print-help))

               ;; ">" — cache last answer
               ((string= trimmed ">")
                (if *last-answer*
                    (let ((action (cache-engine:add_cache *cache* *last-answer*)))
                      (format t "  [~A ~D items total]~%"
                              (if (eq action :refreshed)
                                  "Already cached, timestamp refreshed."
                                  "Cached.")
                              (cache-engine:count-items *cache*)))
                    (format t "  [No answer to cache yet]~%")))

               ;; "!" alone — clear old cache
               ((string= trimmed "!")
                (clear-old-cache))

               ;; "!<query>" — search-grounded question
               ((char= (char trimmed 0) #\!)
                (format t "  [Searching...]~%")
                (finish-output)
                (multiple-value-bind (answer error-p)
                    (ask-gemini (string-trim *whitespace* (subseq trimmed 1))
                                :search-p t)
                  (display-answer answer error-p)))

               ;; Plain question
               (t
                (format t "  [Thinking...]~%")
                (finish-output)
                (multiple-value-bind (answer error-p) (ask-gemini trimmed)
                  (display-answer answer error-p)))))))
    (save-history)))

;;; ---- Entry point ----

(defun main ()
  "Initialize cache and start the REPL."
  (setf *cache* (make-instance 'cache-engine:cache-engine
                               :db-path (namestring *cache-db-path*)))
  (setf *last-answer* nil)
  (unwind-protect
       (repl-loop)
    (cache-engine:close-cache *cache*)
    (setf *cache* nil)
    (format t "  [Cache closed]~%")))
