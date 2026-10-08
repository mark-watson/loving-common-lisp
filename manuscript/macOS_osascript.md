# macOS Automation with AppleScript and SQLite - macOS Only

Apple provides system hooks to automate macOS desktop applications through Apple Events. The command-line program `osascript` runs AppleScript code and JavaScript for Automation (JXA) scripts from shell sessions and sub-processes. Common Lisp programs can call `osascript` through UIOP to control native macOS applications, including Mail, Calendar, Messages, Reminders, and Safari. In addition, Common Lisp can read historical chat records directly by querying the local SQLite database that stores iMessage data.

This chapter presents the `:mac` system. It shows how to automate macOS applications and read system data without external dependencies beyond UIOP.

**Note 1: This code runs only on macOS.**

**Note 2: This code requires SBCL and UIOP.**

## Security and System Permissions (TCC)

macOS enforces a privacy framework called Transparency, Consent, and Control (TCC). TCC protects sensitive user data such as mailboxes, calendars, contacts, and message logs. Before you run this code, you must grant the appropriate permissions to the terminal emulator or text editor hosting your Common Lisp environment.

### 1. Automation Permissions

When a Lisp process controls another application through `osascript`, macOS prompts the user for consent. If you do not grant permission, or if macOS suppresses the prompt, Apple Events fail with permission errors.

To check or update these permissions:

1. Open **System Settings > Privacy & Security > Automation**.
2. Locate the host application running your Common Lisp image (such as Terminal, iTerm2, or Emacs).
3. Confirm that toggle switches for Mail, Calendar, Messages, Reminders, and Safari are active.

### 2. Full Disk Access for Message History

The Messages application stores chat history in an SQLite database located at `~/Library/Messages/chat.db`. Because this database contains personal communication records, macOS blocks regular file access even if the user owns the file.

To allow Common Lisp to read this database:

1. Open **System Settings > Privacy & Security > Full Disk Access**.
2. Add your terminal emulator or editor to the permitted applications list.
3. Enable the toggle switch.

If you omit this step, queries against `chat.db` fail with an `unable to open database file` error.

### 3. Safari Apple Events Permission

Safari blocks script execution from Apple Events by default. If a script attempts to run JavaScript inside Safari without this setting, macOS returns error `-10004` (privilege violation).

To enable JavaScript execution in Safari:

1. Launch Safari.
2. Open **Settings > Advanced** and check **Show features for web developers** (or **Show Develop menu in menu bar** on older macOS versions).
3. Select the **Develop** menu in the menu bar.
4. Check **Allow JavaScript from Apple Events**.

You can also set this preference from the terminal:

```bash
defaults write -app Safari AllowJavaScriptFromAppleEvents 1
```

## System Definition and Package Structure

The system definition lives in `mac.asd`. It depends only on `#:uiop` and loads four files in sequence:

```lisp
;;;; mac.asd -- ASDF system for the :mac package.

(asdf:defsystem #:mac
  :description "macOS automation via osascript (Mail, Calendar, Messages, Reminders, Safari)."
  :depends-on (#:uiop)
  :serial t
  :components ((:file "package")
               (:file "osascript")
               (:file "read-recent-imessages")
               (:file "safari-login")))
```

The package definition lives in `package.lisp`. It exports the public functions and condition symbols:

```lisp
;;;; package.lisp -- the :mac package shared by all files in this directory.
;;;; Load this file first.

(defpackage :mac
  (:use :cl)
  (:export
   ;; osascript.lisp
   #:osascript-error
   #:osascript-error-code
   #:osascript-error-message
   #:osascript-error-script
   #:run-osascript
   #:mail-unread-count
   #:get-todays-events
   #:send-imessage
   #:add-reminder
   ;; read-recent-imessages.lisp
   #:read-recent-imessages
   ;; safari-login.lisp
   #:safari-login))
```

## The AppleScript Interface (`osascript.lisp`)

The file `osascript.lisp` provides the execution core and standard application handlers.

### Capturing Subprocess Errors

When `osascript` fails, it prints diagnostics to standard error (such as syntax errors or missing permissions) and exits with status 1. Standard calls to `uiop:run-program` drop standard error on failures and signal a generic `uiop:subprocess-error`.

To preserve the error text, `osascript.lisp` defines a dedicated condition type `osascript-error`:

```lisp
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
```

The function `run-osascript` runs the script with `osascript -e`, captures standard error, and inspects the return code:

```lisp
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
```

### Reading Unread Mail

The function `mail-unread-count` asks Mail for the unread message count of the inbox:

```lisp
(defun mail-unread-count ()
  (parse-integer (run-osascript "tell application \"Mail\" to return unread count of inbox")))
```

`osascript` returns the number as a string on standard output. `parse-integer` parses this output into a Lisp integer.

### Fetching Calendar Events

The function `get-todays-events` queries Calendar for events scheduled between midnight of the current day and midnight of the following day:

```lisp
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
```

AppleScript uses the global property `AppleScript's text item delimiters` to join list items when casting a list to a string. Setting the delimiter to `"|"` allows Lisp to convert the output into a list of strings using `uiop:split-string`.

### Sending Messages and Creating Reminders

`send-imessage` targets the Messages service to deliver text to a recipient:

```lisp
(defun send-imessage (phone-number message)
  ;; ~S wraps the Lisp strings in the double quotes AppleScript needs.
  (let ((script (format nil "tell application \"Messages\"
  set targetService to 1st service whose service type = iMessage
  set targetBuddy to buddy ~S of targetService
  send ~S to targetBuddy
end tell" phone-number message)))
    (run-osascript script)))
```

The format directive `~S` places quotes around the phone number and message text, ensuring valid AppleScript syntax.

`add-reminder` creates a new entry in Reminders and returns the unique reminder identifier:

```lisp
(defun add-reminder (task-name)
  (let ((script (format nil "tell application \"Reminders\"
  set newReminder to make new reminder with properties {name:~S}
  return id of newReminder
end tell" task-name)))
    (run-osascript script)))
```

## Reading iMessage History via SQLite (`read-recent-imessages.lisp`)

AppleScript does not provide a fast, searchable interface for reading message histories. The Messages application stores message data in an SQLite database at `~/Library/Messages/chat.db`. Reading this file directly with the command-line utility `sqlite3` provides fast access to message records.

The function `read-recent-imessages` joins the `message` table with the `handle` table:

```lisp
(defun read-recent-imessages (&optional (limit 5))
  (let* ((db-path (namestring (merge-pathnames "Library/Messages/chat.db" 
                                               (user-homedir-pathname))))
         ;; Join the message table with the handle table to get the sender's phone/email
         (query (format nil "SELECT m.is_from_me, h.id AS sender, m.text 
                             FROM message m 
                             LEFT JOIN handle h ON m.handle_id = h.ROWID 
                             WHERE m.text IS NOT NULL 
                             ORDER BY m.date DESC LIMIT ~A;" 
                        limit))
         ;; The -json flag outputs a cleanly formatted JSON array of objects
         (command (list "sqlite3" "-json" db-path query)))
    
    ;; Returns a JSON string suitable for parsing with shasht, jonathan, or cl-json
    (uiop:run-program command :output :string)))
```

The SQL query extracts three fields:
- `m.is_from_me`: an integer (1 if sent by the user, 0 if received).
- `h.id`: the identifier of the sender (phone number or Apple Account email).
- `m.text`: the text body of the message.

The `-json` argument instructs `sqlite3` to output a JSON array of objects. Common Lisp programs can pass this string directly to a JSON parser such as `cl-json` or `shasht`.

## Browser Automation with Safari (`safari-login.lisp`)

The file `safari-login.lisp` provides browser automation without external browser drivers. It controls Safari through AppleScript's `do JavaScript` command.

The file is self-contained. It can load alongside `osascript.lisp` or independently.

### Escaping JavaScript Values

The function `safari-js-string` formats strings for JavaScript string literals. The format control `~S` escapes internal double quotes:

```lisp
(defun safari-js-string (value)
  "VALUE as a double-quoted JavaScript string literal.  ~S yields \\\" for
embedded quotes, which is valid in JavaScript *and* in the AppleScript
string literal that will carry it."
  (format nil "~S" (string value)))
```

The function `safari-js-single-quoted` produces single-quoted literals for HTML element identifiers:

```lisp
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
```

### Form Submission and Polling

The function `safari-login` opens a target URL, waits for the web page to finish loading, and injects JavaScript to fill and submit the form:

```lisp
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
```

The script polls `document.readyState` every 0.5 seconds until it returns `"complete"`, with a default timeout limit of 30 seconds (`timeout = 30`). Once the document loads, the script sets form field values and invokes the form's `submit()` method.

## Running the Code in the REPL

Start SBCL in the `src/macOS_osascript/` directory:

```bash
cd src/macOS_osascript
sbcl
```

Load the system using ASDF:

```lisp
(require :asdf)
(asdf:load-asd (truename "mac.asd"))
(asdf:load-system :mac)
```

Alternatively, load the individual files directly:

```lisp
(load "package.lisp")
(load "osascript.lisp")
(load "read-recent-imessages.lisp")
(load "safari-login.lisp")
```

Enter the `:mac` package or call functions with the `mac:` package prefix.

### Querying Unread Mail

```lisp
(mac:mail-unread-count)
;; => 3
```

### Listing Today's Calendar Events

```lisp
(mac:get-todays-events)
;; => ("Team Sync" "Project Review" "Dentist Appointment")
```

### Adding a Reminder

```lisp
(mac:add-reminder "Buy groceries")
;; => "x-apple-reminder://..."
```

### Sending an iMessage

```lisp
(mac:send-imessage "+15551234567" "Hello from Common Lisp!")
;; => ""
```

### Inspecting Recent Messages

Read the last three messages from the local SQLite store:

```lisp
(mac:read-recent-imessages 3)
;; => "[{\"is_from_me\":0,\"sender\":\"+15551234567\",\"text\":\"See you at lunch\"},...]"
```

### Automating Safari Login

```lisp
(mac:safari-login "https://example.com/login"
                  "user@example.com"
                  "secret-pass"
                  :username-id "user-field"
                  :password-id "pass-field"
                  :form-id "login-form")
```

## Summary of Error Codes and Troubleshooting

| Error Symptom | Cause | Solution |
| --- | --- | --- |
| `-10004 privilege violation` in Safari | Safari blocks Apple Events from executing JavaScript. | Enable **Safari > Develop > Allow JavaScript from Apple Events**. |
| `Not authorized to send Apple events to System Events / App` | TCC blocks Apple Events between the terminal/editor and the target application. | Enable the host terminal or editor in **System Settings > Privacy & Security > Automation**. |
| `unable to open database file` on `chat.db` | TCC blocks access to `~/Library/Messages/chat.db`. | Grant **Full Disk Access** to your terminal or editor in **System Settings > Privacy & Security > Full Disk Access**. |
| `Safari page did not reach readyState 'complete'` | Page load timed out before DOM completed loading. | Increase the `:timeout` key argument, or check network connectivity. |
| `null is not an object` in Safari JavaScript | Element identifier does not exist on the target web page. | Check HTML source and verify `:username-id`, `:password-id`, and `:form-id` values. |
