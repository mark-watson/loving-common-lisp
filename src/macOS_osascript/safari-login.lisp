;;;; safari-login.lisp -- Safari automation, kept separate from the general
;;;; osascript bridge in osascript.lisp.
;;;;
;;;; Self-contained on purpose: loading this file alone gives you everything
;;;; it needs, and its private helpers are all named SAFARI-* so that loading
;;;; it alongside osascript.lisp cannot redefine anything.  The two files are
;;;; designed to co-exist: load either one, or both, in any order.  When
;;;; osascript.lisp is also loaded, failures here signal its OSASCRIPT-ERROR
;;;; condition, so one HANDLER-CASE covers both files.
;;;;
;;;; Loading this file only *defines* functions; it never talks to an app.
;;;; Call it explicitly, e.g.:
;;;;
;;;;   (load "safari-login.lisp")
;;;;   (safari-login "https://leanpub.com/author/royalties/summary"
;;;;                 (uiop:getenv "LEANPUB_USER")
;;;;                 (uiop:getenv "LEANPUB_PASSWORD"))
;;;;
;;;; Keep credentials OUT of this file: it lives in a git repo.
;;;;
;;;; macOS requirements, both of which fail with "-10004 privilege violation":
;;;;   1. Safari > Develop > Allow JavaScript from Apple Events
;;;;      (scriptable equivalent: defaults write -app Safari \
;;;;                             AllowJavaScriptFromAppleEvents 1)
;;;;   2. System Settings > Privacy & Security > Automation: allow your
;;;;      terminal / Emacs to control Safari.
;;;; Also: USERNAME-ID / PASSWORD-ID / FORM-ID must match the target page's
;;;; actual HTML, or the JavaScript throws on a null element.

(in-package :mac)

;;; ------------------------------------------------------------------
;;; Private osascript runner.  osascript writes "execution error: ..." to
;;; stderr and exits 1; with UIOP's default error handling that stderr is
;;; thrown away and all you see is an opaque exit code, so capture it.
;;; ------------------------------------------------------------------

(defun safari-signal-failure (exit-code script message)
  "Report a failed osascript run.  If osascript.lisp is loaded, signal its
OSASCRIPT-ERROR condition (so a single HANDLER-CASE covers Safari and the
Mail/Calendar/Reminders helpers alike); otherwise fall back to a plain error
carrying the same text, keeping this file usable on its own."
  (if (find-class 'osascript-error nil)
      (error 'osascript-error :code exit-code :script script :message message)
      (error "osascript failed (exit ~D): ~A" exit-code message)))

(defun safari-run-osascript (script)
  "Run AppleScript source SCRIPT with `osascript -e', returning trimmed
stdout.  Signals an error whose message is osascript's own diagnostic."
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (list "osascript" "-e" script)
                        :output :string
                        :error-output :string   ; <-- the real error text
                        :ignore-error-status t) ; <-- inspect the code ourselves
    (let ((out (string-trim '(#\Space #\Newline #\Tab) (or stdout "")))
          (err (string-trim '(#\Space #\Newline #\Tab) (or stderr ""))))
      (if (zerop exit-code)
          out
          (safari-signal-failure exit-code script
                                 (cond ((plusp (length err)) err)
                                       ((plusp (length out)) out)
                                       (t "no diagnostic on stderr")))))))

;;; ------------------------------------------------------------------
;;; JavaScript string literals
;;; ------------------------------------------------------------------

(defun safari-js-string (value)
  "VALUE as a double-quoted JavaScript string literal.  ~S yields \\\" for
embedded quotes, which is valid in JavaScript *and* in the AppleScript
string literal that will carry it."
  (format nil "~S" (string value)))

(defun safari-js-single-quoted (value)
  "VALUE as a single-quoted JavaScript string literal (for element ids)."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for character across (string value)
          do (case character
               (#\' (write-string "\\'" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (t (write-char character out))))
    (write-char #\' out)))

;;; ------------------------------------------------------------------
;;; Safari
;;; ------------------------------------------------------------------

(defun safari-login (url username password
                     &key (username-id "username")
                          (password-id "password")
                          (form-id "login-form")
                          (timeout 30))
  "Open URL in Safari, wait for the page, then fill and submit the login
form.  USERNAME-ID / PASSWORD-ID / FORM-ID must match the target site's
HTML.  TIMEOUT is the maximum number of seconds to wait for the page."
  (let* ((javascript
           (format nil "document.getElementById(~A).value = ~A; document.getElementById(~A).value = ~A; document.getElementById(~A).submit();"
                   (safari-js-single-quoted username-id) (safari-js-string username)
                   (safari-js-single-quoted password-id) (safari-js-string password)
                   (safari-js-single-quoted form-id)))
         ;; One AppleScript string literal carrying the whole JavaScript
         ;; program.  Do NOT build the JavaScript by concatenating AppleScript
         ;; strings ("... = " & "value"): that drops the JavaScript quotes and
         ;; yields invalid JavaScript such as
         ;;   document.getElementById('username').value = markw@example.com
         (script
           (format nil "set targetURL to ~S
set pageLoaded to false

tell application \"Safari\"
  set targetDoc to make new document with properties {URL:targetURL}

  -- Poll for the page, but with a hard cap so we can never hang forever.
  set waited to 0.0
  repeat while waited < ~,1F
    delay 0.5
    set waited to waited + 0.5
    if (do JavaScript \"document.readyState\" in targetDoc) is \"complete\" then
      set pageLoaded to true
      exit repeat
    end if
  end repeat

  if pageLoaded then do JavaScript ~S in targetDoc
end tell

if not pageLoaded then error \"Safari page did not reach readyState 'complete' within ~,1F seconds\""
                   url timeout javascript timeout)))
    (safari-run-osascript script)))
