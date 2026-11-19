;;;; mac-tools.lisp -- MCP tools built on the :mac osascript functions.

(in-package #:mcp-server)

(deftool mail-unread-count ()
  "Return the number of unread messages in the macOS Mail inbox."
  (format nil "~D unread messages" (mac:mail-unread-count)))

(deftool todays-events ()
  "List the titles of today's events from all calendars in macOS Calendar."
  (let ((events (remove "" (mac:get-todays-events) :test #'string=)))
    (if events
        (format nil "~{~A~^~%~}" events)
        "No events today.")))

(deftool add-reminder ((task-name "string" "Text of the new reminder" :required t))
  "Create a new reminder in the macOS Reminders app."
  (format nil "Created reminder ~S (id ~A)" task-name (mac:add-reminder task-name)))

(deftool read-recent-imessages
    ((limit "integer" "How many recent messages to return (1 to 50)" :default 5))
  "Read the most recent iMessages as JSON objects with is_from_me, sender,
and text fields. Requires Full Disk Access for the process running the server."
  (unless (<= 1 limit 50)
    (error "limit must be between 1 and 50"))
  (mac:read-recent-imessages limit))

(deftool send-imessage
    ((phone-number "string" "Recipient phone number or Apple ID email" :required t)
     (message "string" "Text to send" :required t))
  "Send an iMessage using the macOS Messages app."
  (mac:send-imessage phone-number message)
  (format nil "Sent message to ~A" phone-number))
