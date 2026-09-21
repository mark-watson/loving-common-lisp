;;; agent.lisp -- Core agent logic for cl-ai-coding-agent
(in-package :cl-ai-coding-agent)

(defvar *verbose* nil
  "When non-NIL, print debug information during
   agent execution.")

(defparameter *max-tool-rounds* 10
  "Maximum number of tool-use round-trips before
   the agent returns whatever it has.")

(defparameter *default-model* "ollama/qwen3.5:4b"
  "Default litelm model for the agent.
   Uses a local Ollama model, so no API key
   is needed.")

;;; ---- Stacktrace detection ----

(defparameter *stacktrace-patterns*
  '("Backtrace"
    "BACKTRACE"
    "debugger invoked"
    "Unhandled"
    "HANDLER-BIND"
    "is not of type"
    "UNDEFINED-FUNCTION"
    "SIMPLE-ERROR"
    "PROGRAM-ERROR"
    "TYPE-ERROR"
    "UNBOUND-VARIABLE"
    "SB-INT:SIMPLE-READER-ERROR"
    "Traceback (most recent call last)"
    "Exception in thread"
    "Error:"
    "Stack trace:")
  "Literal substrings -- not regular expressions -- that
   indicate the input contains a stacktrace or error
   message.  STACKTRACE-P searches with SEARCH, so a pattern
   written as a regex would silently never match.")

(defun stacktrace-p (text)
  "Return T if TEXT likely contains a stacktrace
   or Common Lisp error output, and NIL otherwise."
  ;; SOME returns the value SEARCH produced, which is a match
  ;; position, so coerce it to a real boolean.
  (and (some (lambda (pat)
               (search pat text :test #'char-equal))
             *stacktrace-patterns*)
       t))

;;; ---- System prompt construction ----

(defun %system-instructions (user-prompt)
  "Build the system message for USER-PROMPT."
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
use write_file, do not just print the code.

~A" stacktrace-instructions)))

;;; ---- Agent loop ----

(defun coding-agent-query (prompt &key (model *default-model*))
  "Process PROMPT through the AI coding agent.
   The agent can read directories, read files,
   write new files, and diagnose stacktraces.
   MODEL is a litelm \"provider/model\" string,
   defaulting to *DEFAULT-MODEL*.
   Returns the final text response."
  (let* ((tools (%make-tool-declarations))
         (messages
          (list (list :system
                      (%system-instructions prompt))
                (list :user prompt)))
         (round 0)
         text calls)
    (when *verbose*
      (format t "~&[coding-agent] model: ~A~%~A~%"
              model messages))
    (loop
      (incf round)
      (let ((resp (litelm:completion model
                                     :messages messages
                                     :tools tools)))
        (setf text (litelm:response-content resp)
              calls (litelm:response-tool-calls resp))
        (when *verbose*
          (format t "[coding-agent] round-~D text: ~A~%"
                  round text)
          (format t "[coding-agent] round-~D calls: ~A~%"
                  round calls))
        (unless calls
          (return (or text "(no response from model)")))
        (when (> round *max-tool-rounds*)
          (return (or text "(agent exhausted tool rounds)")))
        (setf messages
              (nconc messages
                     (list (list :assistant text
                                 :tool-calls calls))
                     (mapcar (lambda (fc)
                               (let ((result
                                      (dispatch-tool-call fc)))
                                 (when *verbose*
                                   (format t
                                    "[coding-agent] tool ~A -> ~A~%"
                                    (getf fc :name)
                                    (subseq result 0
                                     (min 200
                                      (length result)))))
                                 (list :tool result
                                       :tool-call-id
                                       (getf fc :id))))
                             calls)))))))

;;; ---- File-based query ----

(defun coding-agent-query-file (path
                                &optional prefix
                                (model *default-model*))
  "Read the contents of PATH and send them as a
   prompt to the coding agent.  This is the easiest
   way to diagnose multi-line stacktraces that may
   contain quote characters -- just save the error
   output to a file and pass the path here.
   PREFIX is an optional string prepended to the
   file contents (e.g. \"Fix this error:\").
   MODEL is a litelm \"provider/model\" string."
  (let* ((content (uiop:read-file-string path))
         (prompt (if prefix
                     (format nil "~A~%~A"
                             prefix content)
                     content)))
    (coding-agent-query prompt :model model)))

;;; ---- Interactive REPL ----

(defparameter *whitespace* '(#\Space #\Tab #\Newline #\Return)
  "Characters trimmed from REPL input.")

(defun coding-agent-repl ()
  "Start an interactive REPL for the coding agent.
   Type 'quit' or 'exit' to leave."
  (format t "~&AI Coding Agent (type quit to exit)~%")
  (loop
    (format t "~&> ")
    (finish-output)
    (let ((input (read-line *standard-input* nil nil)))
      (when (null input)
        (format t "~&Goodbye.~%")
        (return))
      (let ((trimmed (string-trim *whitespace* input)))
        (cond
          ((member trimmed '("quit" "exit")
                   :test #'string-equal)
           (format t "~&Goodbye.~%")
           (return))
          ((plusp (length trimmed))
           (let ((response
                  (handler-case
                      (coding-agent-query trimmed)
                    (error (e)
                      (format nil "Error: ~A" e)))))
             (format t "~&~A~%" response))))))))
