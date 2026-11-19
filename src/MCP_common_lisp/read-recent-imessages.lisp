(in-package :mac)

;; Read iMessage data
;;
;; Because chat.db contains sensitive user data, macOS heavily restricts access to it via
;; Transparency, Consent, and Control (TCC).
;;
;; Before this function will execute successfully, you must grant Full Disk Access to the
;; host environment running your Lisp image.
;;
;; Open System Settings > Privacy & Security > Full Disk Access.
;;
;; Add your Terminal application (e.g., iTerm2, Terminal.app) or your specific IDE (like Emacs,
;; if you are running SLIME/Sly directly from it).
;;
;; If you do not grant this permission, the sqlite3 call will fail silently or return an "unable to open database file" error.
;;

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
