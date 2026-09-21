;;; bridge.lisp — JS ↔ Lisp bridge for webkit-cl
;;;
;;; Manages command handlers and dispatches bridge invocations.
;;; Inspired by zero-native's bridge architecture but simplified
;;; for Common Lisp ergonomics.

(in-package #:webkit-cl)

;;; ── Handler Registry ───────────────────────────────────────────

(defvar *bridge-handlers* (make-hash-table :test 'equal)
  "Hash table mapping command names (strings) to handler functions.
   Each handler receives a parsed JSON payload (alist) and returns
   a string (JSON) to send back to JavaScript.")

(defun register-handler (command handler-fn)
  "Register a bridge handler for COMMAND.
   HANDLER-FN takes one argument: the parsed payload (alist from cl-json).
   It should return a JSON string to send back to JavaScript,
   or NIL for no response.

   Example:
     (register-handler \"greet\"
       (lambda (payload)
         (format nil \"{\\\"message\\\": \\\"Hello, ~a!\\\"}\"
                 (cdr (assoc :name payload)))))"
  (setf (gethash command *bridge-handlers*) handler-fn)
  command)

(defun unregister-handler (command)
  "Remove the bridge handler for COMMAND."
  (remhash command *bridge-handlers*)
  command)

;;; ── Dispatch ───────────────────────────────────────────────────

(defun dispatch-bridge-command (command payload-json)
  "Dispatch a bridge command to the registered handler.
   Called from the CFFI bridge-dispatch callback.
   Returns a JSON string, or NIL."
  (let ((handler (gethash command *bridge-handlers*)))
    (if handler
        (handler-case
            (let* ((payload (handler-case
                                (json:decode-json-from-string payload-json)
                              (error () nil)))
                   (result (funcall handler payload)))
              (if result
                  result
                  "null"))
          (error (e)
            (format nil "{\"error\": \"~a\"}"
                    (substitute-json-chars (format nil "~a" e)))))
        (format nil "{\"error\": \"unknown command: ~a\"}"
                (substitute-json-chars command)))))

(defun substitute-json-chars (str)
  "Escape characters that are special in JSON strings."
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (write-char ch out))))))

;;; ── Convenience for building JSON responses ────────────────────

(defun json-response (&rest pairs)
  "Build a JSON object string from keyword/value PAIRS.
   Values are encoded with cl-json.

   Example:
     (json-response :message \"hello\" :count 42)
     => {\"message\":\"hello\",\"count\":42}

   Note: build the object with cl-json's streaming API rather than by
   handing it an alist.  An alist entry whose value is NIL collapses to
   the one-element list (KEY), which cl-json encodes as a JSON *array*
   instead of an object member — so (json-response :a nil) would come out
   as [[\"a\"]] rather than {\"a\":null}."
  (assert (evenp (length pairs)) (pairs)
          "JSON-RESPONSE needs an even number of arguments, got ~d."
          (length pairs))
  (with-output-to-string (out)
    (json:with-object (out)
      (loop for (key val) on pairs by #'cddr
            do (json:as-object-member (key out)
                 (json:encode-json val out))))))
