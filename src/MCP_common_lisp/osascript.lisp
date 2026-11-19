;;;; osascript.lisp -- small AppleScript bridge for SBCL/UIOP.
;;;;
;;;; Loading this file only *defines* functions; it never talks to an app.
;;;; Call something explicitly, e.g.:
;;;;
;;;;   (load "osascript.lisp")
;;;;   (mail-unread-count)
;;;;   (get-todays-events)
;;;;
;;;; Safari automation deliberately lives in its own file, safari-login.lisp,
;;;; so it can be ignored until you need it again.
;;;;
;;;; Keep credentials OUT of this file: it lives in a git repo.

(in-package :mac)

;;; ------------------------------------------------------------------
;;; Error reporting.  osascript writes "execution error: ..." to stderr
;;; and exits 1; with UIOP's default error handling that stderr is
;;; thrown away and you only see an opaque SUBPROCESS-ERROR.  Capture
;;; it and re-signal something that actually names the problem.
;;; ------------------------------------------------------------------

(define-condition osascript-error (error)
  ((code    :initarg :code    :reader osascript-error-code)
   (message :initarg :message :reader osascript-error-message)
   (script  :initarg :script  :reader osascript-error-script))
  (:report (lambda (condition stream)
             (format stream "osascript failed (exit ~D): ~A"
                     (osascript-error-code condition)
                     (osascript-error-message condition))))
  (:documentation "Signalled when osascript exits non-zero.  The MESSAGE
slot holds the real AppleScript diagnostic from stderr."))

(defun run-osascript (script)
  "Run AppleScript source SCRIPT with `osascript -e', returning trimmed
stdout.  Signals OSASCRIPT-ERROR (with osascript's own message) on failure."
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (list "osascript" "-e" script)
                        :output :string
                        :error-output :string   ; <-- the real error text
                        :ignore-error-status t) ; <-- inspect the code ourselves
    (let ((out (string-trim '(#\Space #\Newline #\Tab) (or stdout "")))
          (err (string-trim '(#\Space #\Newline #\Tab) (or stderr ""))))
      (if (zerop exit-code)
          out
          (error 'osascript-error
                 :code exit-code
                 :script script
                 :message (if (plusp (length err))
                              err
                              (if (plusp (length out)) out "no diagnostic on stderr")))))))

;;; ------------------------------------------------------------------
;;; Mail / Calendar / Messages / Reminders
;;; ------------------------------------------------------------------

(defun mail-unread-count ()
  (parse-integer (run-osascript "tell application \"Mail\" to return unread count of inbox")))

(defun get-todays-events ()
  (let ((script "tell application \"Calendar\"
  set todayStart to (current date)
  set time of todayStart to 0
  set todayEnd to todayStart + 1 * days
  set eventList to {}

  repeat with c in calendars
    set calendarEvents to (summary of (every event of c whose start date >= todayStart and start date < todayEnd))
    repeat with e in calendarEvents
      set end of eventList to e
    end repeat
  end repeat

  set AppleScript's text item delimiters to \"|\"
  return eventList as string
end tell"))
    ;; Split the pipe-delimited string directly into a Common Lisp list
    (uiop:split-string (run-osascript script) :separator '(#\|))))

(defun send-imessage (phone-number message)
  ;; ~S wraps the Lisp strings in the double quotes AppleScript needs.
  (let ((script (format nil "tell application \"Messages\"
  set targetService to 1st service whose service type = iMessage
  set targetBuddy to buddy ~S of targetService
  send ~S to targetBuddy
end tell" phone-number message)))
    (run-osascript script)))

(defun add-reminder (task-name)
  (let ((script (format nil "tell application \"Reminders\"
  set newReminder to make new reminder with properties {name:~S}
  return id of newReminder
end tell" task-name)))
    (run-osascript script)))
