;;;; daily-use.lisp — Interactive Gemini REPL with search grounding and cache
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

(defvar *model* "gemini-3.1-flash-lite")
(defvar *cache-db-path*
  (merge-pathnames ".daily-use-cache.db" (user-homedir-pathname)))

;;; ---- State ----

(defvar *cache* nil
  "The cache-engine instance for persisting useful answers.")
(defvar *last-answer* nil
  "The last answer returned by Gemini, available for caching with '>'.")

;;; ---- Cache context builder ----

(defun build-context-from-cache ()
  "Retrieves all cached items and builds a context string for the LLM prompt."
  (let ((items (cache-engine:lookup *cache* nil :limit 50)))
    (if items
        (format nil "Use the following context from previous conversations when answering:~%~%~{- ~A~%~}~%---~%~%"
                items)
        "")))

;;; ---- Query dispatch ----

(defun ask-gemini (prompt &key search-p)
  "Sends PROMPT to Gemini, optionally with Google Search grounding.
   Prepends cached context to the prompt."
  (let* ((context (build-context-from-cache))
         (full-prompt (concatenate 'string context prompt)))
    (handler-case
        (if search-p
            (gemini:generate-with-search full-prompt *model*)
            (gemini:generate full-prompt *model*))
      (error (c)
        (format nil "[Error calling Gemini API: ~A]" c)))))

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

(defun display-answer (text)
  "Prints the Gemini answer with a visual separator."
  (if text
      (progn
        (format t "~%~A~%~%" text)
        (setf *last-answer* text))
      (format t "~%  [No response from Gemini — check model name or API key]~%~%")))

;;; ---- REPL ----

(defun repl-loop ()
  "Main REPL loop with cl-readline for line editing and history."
  (format t "~%  Gemini Daily-Use REPL  (type 'h' for help)~%~%")
  (loop
    (let ((input (rl:readline :prompt "gemini> " :add-history t)))
      ;; Handle EOF (Ctrl-D)
      (when (null input)
        (format t "~%Goodbye.~%")
        (return))

      (let ((trimmed (string-trim '(#\Space #\Tab) input)))
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
               (progn
                 (cache-engine:add_cache *cache* *last-answer*)
                 (format t "  [Cached. ~D items total]~%"
                         (cache-engine:count-items *cache*)))
               (format t "  [No answer to cache yet]~%")))

          ;; "!" alone — clear old cache
          ((string= trimmed "!")
           (let ((before (cache-engine:count-items *cache*)))
             (cache-engine:clear-cache-older-one-week *cache*)
             (let ((after (cache-engine:count-items *cache*)))
               (format t "  [Cleared ~D old entries. ~D items remain]~%"
                       (- before after) after))))

          ;; "!<query>" — search-grounded question
          ((char= (char trimmed 0) #\!)
           (let ((query (string-trim '(#\Space #\Tab) (subseq trimmed 1))))
             (if (string= query "")
                 ;; Edge case: "! " with only whitespace after
                 (let ((before (cache-engine:count-items *cache*)))
                   (cache-engine:clear-cache-older-one-week *cache*)
                   (let ((after (cache-engine:count-items *cache*)))
                     (format t "  [Cleared ~D old entries. ~D items remain]~%"
                             (- before after) after)))
                 (progn
                   (format t "  [Searching...]~%")
                   (finish-output)
                   (display-answer (ask-gemini query :search-p t))))))

          ;; Plain question
          (t
           (format t "  [Thinking...]~%")
           (finish-output)
           (display-answer (ask-gemini trimmed))))))))

;;; ---- Entry point ----

(defun main ()
  "Initialize cache and start the REPL."
  (setf *cache* (make-instance 'cache-engine:cache-engine
                               :db-path (namestring *cache-db-path*)))
  (setf *last-answer* nil)
  (unwind-protect
       (repl-loop)
    (cache-engine:close-cache *cache*)
    (format t "  [Cache closed]~%")))
