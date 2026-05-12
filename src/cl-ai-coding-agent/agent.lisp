;;; agent.lisp -- Core agent logic for cl-ai-coding-agent
(in-package :cl-ai-coding-agent)

(defvar *verbose* nil
  "When non-NIL, print debug information during
   agent execution.")

(defparameter *max-tool-rounds* 10
  "Maximum number of tool-use round-trips before
   the agent returns whatever it has.")

;;; ---- Stacktrace detection ----

(defparameter *stacktrace-patterns*
  '("Backtrace"
    "BACKTRACE"
    "debugger invoked"
    "Unhandled"
    "HANDLER-BIND"
    "The value"
    "is not of type"
    "UNDEFINED-FUNCTION"
    "SIMPLE-ERROR"
    "PROGRAM-ERROR"
    "TYPE-ERROR"
    "UNBOUND-VARIABLE"
    "SB-INT:SIMPLE-READER-ERROR"
    "Traceback (most recent call last)"
    "at .* line [0-9]+"
    "Exception in thread"
    "Error:"
    "Stack trace:")
  "Patterns indicating the input contains a
   stacktrace or error message.")

(defun stacktrace-p (text)
  "Return T if TEXT likely contains a stacktrace
   or Common Lisp error output."
  (some (lambda (pat)
          (search pat text :test #'char-equal))
        *stacktrace-patterns*))

;;; ---- System prompt construction ----

(defun %system-prompt (user-prompt)
  "Build the full prompt sent to Gemini, including
   the system instructions and the user's input."
  (let ((stacktrace-instructions
         (if (stacktrace-p user-prompt)
             "The user's input contains a stacktrace
or error message.  Analyze it carefully:
1. Identify the root cause of the error.
2. Explain what went wrong in plain English.
3. Suggest a concrete fix with corrected code.
Only use file-system tools if you genuinely need
to see source code context.  If the error message
is self-explanatory, answer directly without
reading files.

"
             "")))
    (format nil
"You are an expert Common Lisp coding assistant.
You have access to three file-system tools:
  - list_directory: list contents of a directory
  - read_file: read a file's contents
  - write_file: create or overwrite a file

Use these tools when the user asks you to inspect
or modify files.  When creating new files, always
use write_file — do NOT just print the code.

~AUser request:
~A" stacktrace-instructions user-prompt)))

;;; ---- Agent loop ----

(defun coding-agent-query (prompt)
  "Process PROMPT through the AI coding agent.
   The agent can read directories, read files,
   write new files, and diagnose stacktraces.
   Returns the final text response."
  (let* ((declarations (%make-tool-declarations))
         (full-prompt  (%system-prompt prompt)))
    (when *verbose*
      (format t "~&[coding-agent] prompt:~%~A~%"
              full-prompt))
    ;; Turn 1 — send prompt with tools
    (multiple-value-bind (text calls interaction-id)
        (gemini:generate-with-tools
         full-prompt declarations)
      (when *verbose*
        (format t "[coding-agent] turn-1 text: ~A~%"
                text)
        (format t "[coding-agent] turn-1 calls: ~A~%"
                calls))
      ;; If no tool calls, return the text directly
      (unless calls
        (return-from coding-agent-query
          (or text "(no response from model)")))
      ;; Multi-turn tool loop
      (loop for round from 1 to *max-tool-rounds*
            while calls
            do
         (let ((responses
                (mapcar
                 (lambda (fc)
                   (let ((result
                          (dispatch-tool-call fc)))
                     (when *verbose*
                       (format t
                        "[coding-agent] tool ~A → ~A~%"
                        (getf fc :name)
                        (subseq result 0
                         (min 200
                          (length result)))))
                     (list :name (getf fc :name)
                           :id   (getf fc :id)
                           :response result)))
                 calls)))
           (multiple-value-bind
               (next-text next-calls next-iid)
               (gemini:continue-with-function-responses
                interaction-id
                responses
                declarations)
             (when *verbose*
               (format t
                "[coding-agent] round-~D text: ~A~%"
                round next-text)
               (format t
                "[coding-agent] round-~D calls: ~A~%"
                round next-calls))
             (setf text           next-text
                   calls          next-calls
                   interaction-id next-iid)))
         finally
         (return
          (or text
              "(agent exhausted tool rounds)"))))))

;;; ---- Interactive REPL ----

(defun coding-agent-repl ()
  "Start an interactive REPL for the coding agent.
   Type 'quit' or 'exit' to leave."
  (format t "~&AI Coding Agent (type quit to exit)~%")
  (loop
    (format t "~&> ")
    (finish-output)
    (let ((input (read-line *standard-input*
                            nil nil)))
      (when (or (null input)
                (string-equal (string-trim
                               '(#\Space) input)
                              "quit")
                (string-equal (string-trim
                               '(#\Space) input)
                              "exit"))
        (format t "~&Goodbye.~%")
        (return))
      (let ((trimmed (string-trim '(#\Space) input)))
        (when (plusp (length trimmed))
          (let ((response
                 (handler-case
                     (coding-agent-query trimmed)
                   (error (e)
                     (format nil "Error: ~A" e)))))
            (format t "~&~A~%" response)))))))
