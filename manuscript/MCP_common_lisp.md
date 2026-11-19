# Writing a Model Context Protocol (MCP) Server in Common Lisp - macOS Only

The Model Context Protocol (MCP) is an open standard that lets LLM clients call tools that run outside the model. Claude Code, Claude Desktop, and many IDE agents act as MCP clients. When you write an MCP server, every one of these clients can use your tools with no client code changes.

In this chapter we write an MCP server from scratch in about 250 lines of Common Lisp. The server exposes the macOS automation functions from the previous chapter as tools, so an LLM can count unread mail, list today's calendar events, add reminders, and read or send iMessages.

We use no MCP library. The protocol is small, and writing it ourselves shows how it works. The only dependencies are UIOP and the JSON library `jzon`.

**Note 1: The tools in this chapter run only on macOS. The server code in `mcp-server.lisp` is portable.**

**Note 2: The source code is in the directory `src/MCP_common_lisp`.**

## How MCP Works

An MCP server can use one of two transports: stdio or HTTP. We use stdio, the simpler choice for local tools:

1. The client starts the server as a child process.
2. The client writes JSON-RPC 2.0 messages to the server's stdin, one message per line.
3. The server writes one JSON-RPC message per line to stdout.
4. The server may write log text to stderr. Stdout must carry nothing but protocol messages.

JSON-RPC has two kinds of messages from the client. A *request* has an `id` field and needs a response with the same `id`. A *notification* has no `id`, and the server must not respond to it.

A session has three steps:

1. The client sends an `initialize` request with the protocol version it wants. The server replies with the version it will use, its capabilities, and its name.
2. The client sends the `notifications/initialized` notification.
3. The client sends `tools/list` to learn which tools exist, then `tools/call` each time the model wants to run a tool.

Each tool has a name, a description, and a JSON Schema that describes its arguments. The model reads the description and schema to decide when to call the tool and what arguments to pass.

## Project Structure

| File | Purpose |
|------|---------|
| `package.lisp` | The `:mac` package (copied from the macOS chapter) |
| `osascript.lisp` | AppleScript bridge for Mail, Calendar, Messages, and Reminders (copied) |
| `read-recent-imessages.lisp` | Reads iMessage history from SQLite (copied) |
| `safari-login.lisp` | Safari automation (copied, not exposed as a tool) |
| `mcp-package.lisp` | The `:mcp-server` package |
| `mcp-server.lisp` | JSON-RPC handling, the tool registry, and the `deftool` macro |
| `mac-tools.lisp` | MCP tool definitions that call the `:mac` functions |
| `mcp-common-lisp.asd` | ASDF system definition |
| `run-server.sh` | Starts the server so that stdout stays clean |
| `test-session.sh` | Sends a sample session to the server |

The previous chapter explains the four copied files, so we don't list them again here.

### mcp-common-lisp.asd

```lisp
;;;; mcp-common-lisp.asd -- an MCP server that exposes macOS automation tools.

(asdf:defsystem #:mcp-common-lisp
  :description "Model Context Protocol (MCP) server over stdio with macOS tools."
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:com.inuoe.jzon)
  :serial t
  :components ((:file "package")
               (:file "osascript")
               (:file "read-recent-imessages")
               (:file "safari-login")
               (:file "mcp-package")
               (:file "mcp-server")
               (:file "mac-tools")))
```

### mcp-package.lisp

We use a local nickname so the code can write `jzon:parse` instead of the full package name `com.inuoe.jzon`:

```lisp
;;;; mcp-package.lisp -- package for the MCP server.

(defpackage #:mcp-server
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:deftool
           #:start
           #:handle-message
           #:*tools*))
```

## The Server: mcp-server.lisp

We walk through `mcp-server.lisp` in five parts: settings, JSON helpers, the tool registry, JSON-RPC errors, and the MCP methods with the main loop.

### Settings

```lisp
(in-package #:mcp-server)

(defparameter *server-name* "common-lisp-macos")
(defparameter *server-version* "0.1.0")

;; Newest first. We echo the client's version if we support it,
;; otherwise we answer with our newest version.
(defparameter *supported-protocol-versions*
  '("2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05"))
```

MCP names protocol versions by date. The client asks for a version in its `initialize` request. If we support that version we send it back. If not, we reply with our newest version, and the client decides whether it can work with it. The parts of the protocol this server uses have not changed across these four versions.

### JSON Helpers

The `jzon` library maps JSON objects to `equal` hash tables, JSON arrays to vectors, `true` to `t`, `false` to `nil`, and `null` to the symbol `null`. Using hash tables for objects means an empty object `{}` and an empty array `[]` stay distinct, which matters because MCP requires both in different places.

```lisp
(defun obj (&rest keys-and-values)
  "Build a JSON object from alternating string keys and values."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash key table) value))
    table))

(defun json-get (object key &optional default)
  (if (hash-table-p object)
      (gethash key object default)
      default))

(defun log-message (format-control &rest args)
  (format *error-output* "~&[mcp] ~?~%" format-control args)
  (finish-output *error-output*))
```

`obj` builds a JSON object, so `(obj "type" "text" "text" "hello")` becomes `{"type":"text","text":"hello"}`. `json-get` reads a field and returns the default when the value is not an object, which saves a type check each time we read a field the client may have left out. `log-message` writes to stderr. The `~?` directive takes a format string and its argument list, so `log-message` accepts the same arguments as `format`.

### The Tool Registry and the deftool Macro

Each tool is a struct in a hash table keyed by the tool name:

```lisp
(defstruct tool
  name          ; string sent to the client, e.g. "add_reminder"
  description
  input-schema  ; JSON Schema object (hash table)
  function)     ; function of one argument: the arguments hash table

(defvar *tools* (make-hash-table :test #'equal)
  "Map from tool name to TOOL struct.")

(defun lisp-name->json-name (symbol)
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))
```

Lisp names use hyphens and JSON names in MCP tools use underscores, so `lisp-name->json-name` turns `task-name` into `task_name`.

Arguments come from the LLM, so we treat them as untrusted input. `parameter-value` fetches one argument, applies a default when the argument is absent, and checks its type:

```lisp
(defun json-type-p (value json-type)
  (cond ((string= json-type "string") (stringp value))
        ((string= json-type "integer") (integerp value))
        ((string= json-type "number") (realp value))
        ((string= json-type "boolean") (or (eq value t) (eq value nil)))
        (t t)))

(defun parameter-value (arguments json-name json-type requiredp default)
  "Fetch and type check one tool argument. An LLM writes these values, so
never trust them: a string where we expect an integer can turn into SQL
or AppleScript injection further down."
  (multiple-value-bind (value presentp) (gethash json-name arguments)
    (cond ((not presentp)
           (if requiredp
               (error "Missing required argument: ~A" json-name)
               default))
          ((not (json-type-p value json-type))
           (error "Argument ~A must be of type ~A" json-name json-type))
          (t value))))
```

The type check matters here. The function `read-recent-imessages` from the previous chapter pastes its `limit` argument into SQL text. If a model sent the string `"5; DROP TABLE message"` as the limit, the type check rejects it before it reaches SQLite.

`make-input-schema` builds the JSON Schema that `tools/list` sends to the client:

```lisp
(defun make-input-schema (parameters)
  (let ((properties (make-hash-table :test #'equal))
        (required-names '()))
    (dolist (parameter parameters)
      (destructuring-bind (var json-type description &key required default)
          parameter
        (declare (ignore default))
        (let ((json-name (lisp-name->json-name var)))
          (setf (gethash json-name properties)
                (obj "type" json-type "description" description))
          (when required (push json-name required-names)))))
    (obj "type" "object"
         "properties" properties
         "required" (coerce (nreverse required-names) 'vector))))
```

Note the name `required-names`. My first version called this list `required`, and the `&key required` in `destructuring-bind` shadowed it, so `push` added names to the wrong variable and every schema came out with an empty `required` array. The test session at the end of this chapter caught the bug.

The `deftool` macro ties these pieces together. A tool definition lists its parameters in the form `(var json-type description &key required default)`, then a description string, then a body that returns the result text:

```lisp
(defmacro deftool (name (&rest parameters) description &body body)
  "Define an MCP tool. Each parameter is
  (VAR JSON-TYPE DESCRIPTION &key REQUIRED DEFAULT)
VAR is bound to the argument value inside BODY. BODY returns the tool's
text result; a non-string result is converted with PRINC-TO-STRING."
  (let ((arguments (gensym "ARGUMENTS")))
    `(setf (gethash ,(lisp-name->json-name name) *tools*)
           (make-tool
            :name ,(lisp-name->json-name name)
            :description ,description
            :input-schema (make-input-schema ',parameters)
            :function
            (lambda (,arguments)
              (declare (ignorable ,arguments))
              (let* ,(loop for (var json-type nil . options) in parameters
                           collect `(,var (parameter-value
                                           ,arguments
                                           ,(lisp-name->json-name var)
                                           ,json-type
                                           ,(getf options :required)
                                           ,(getf options :default))))
                ,@body))))))
```

The macro computes the tool name at expansion time. It generates a `lambda` that binds each parameter variable to a checked value from the arguments hash table, then runs the body. The `gensym` keeps the arguments variable from clashing with a parameter that a tool author might name `arguments`. You can see the expansion with `macroexpand-1`:

```lisp
(macroexpand-1
 '(deftool add-reminder ((task-name "string" "Text" :required t))
    "Create a reminder."
    (mac:add-reminder task-name)))
```

This prints (after reformatting):

```lisp
(SETF (GETHASH "add_reminder" *TOOLS*)
      (MAKE-TOOL
       :NAME "add_reminder"
       :DESCRIPTION "Create a reminder."
       :INPUT-SCHEMA (MAKE-INPUT-SCHEMA '((TASK-NAME "string" "Text" :REQUIRED T)))
       :FUNCTION
       (LAMBDA (#:ARGUMENTS123)
         (DECLARE (IGNORABLE #:ARGUMENTS123))
         (LET* ((TASK-NAME (PARAMETER-VALUE #:ARGUMENTS123 "task_name"
                                            "string" T NIL)))
           (MAC:ADD-REMINDER TASK-NAME)))))
```

### JSON-RPC Errors

JSON-RPC defines numeric error codes. We use four of them:

| Code | Meaning |
|------|---------|
| -32700 | Parse error: the line is not valid JSON |
| -32600 | Invalid request: no `method` field |
| -32601 | Method not found |
| -32602 | Invalid params, such as an unknown tool name |
| -32603 | Internal error |

A condition class carries the code from deep in the call stack up to the code that writes the response:

```lisp
(define-condition json-rpc-error (error)
  ((code :initarg :code :reader json-rpc-error-code)
   (message :initarg :message :reader json-rpc-error-message))
  (:report (lambda (condition stream)
             (format stream "JSON-RPC error ~D: ~A"
                     (json-rpc-error-code condition)
                     (json-rpc-error-message condition)))))

(defun rpc-error (code format-control &rest args)
  (error 'json-rpc-error :code code
                         :message (apply #'format nil format-control args)))

(defun success-response (id result)
  (obj "jsonrpc" "2.0" "id" id "result" result))

(defun error-response (id code message)
  (obj "jsonrpc" "2.0" "id" id
       "error" (obj "code" code "message" message)))
```

### The MCP Methods

`initialize` picks the protocol version and declares that this server supports tools. Setting `listChanged` to false tells the client our tool list never changes during a session:

```lisp
(defun handle-initialize (params)
  (let* ((requested (json-get params "protocolVersion"))
         (version (if (member requested *supported-protocol-versions*
                              :test #'equal)
                      requested
                      (first *supported-protocol-versions*))))
    (log-message "initialize: client asked for ~A, using ~A" requested version)
    (obj "protocolVersion" version
         "capabilities" (obj "tools" (obj "listChanged" nil))
         "serverInfo" (obj "name" *server-name* "version" *server-version*))))
```

`tools/list` returns every tool sorted by name:

```lisp
(defun handle-tools-list ()
  (let ((tools (loop for tool being the hash-values of *tools*
                     collect (obj "name" (tool-name tool)
                                  "description" (tool-description tool)
                                  "inputSchema" (tool-input-schema tool)))))
    (obj "tools" (coerce (sort tools #'string< :key (lambda (tool) (gethash "name" tool)))
                         'vector))))
```

`tools/call` runs a tool. MCP separates two kinds of failure. A request for an unknown tool is a protocol error, so we signal `json-rpc-error`. A failure inside a tool, such as a missing argument or an AppleScript error, goes back as a normal result with `isError` set to true. The model sees that error text and can fix its arguments and try again:

```lisp
(defun tool-result (text &optional errorp)
  (obj "content" (vector (obj "type" "text" "text" text))
       "isError" errorp))

(defun handle-tools-call (params)
  (let* ((name (json-get params "name"))
         (arguments (json-get params "arguments"))
         (tool (and (stringp name) (gethash name *tools*))))
    (unless tool
      (rpc-error -32602 "Unknown tool: ~A" name))
    (unless (hash-table-p arguments)
      (setf arguments (make-hash-table :test #'equal)))
    (log-message "tools/call ~A" name)
    ;; MCP reports failures inside a tool as a normal result with isError
    ;; true, so the model can read the message and try again.
    (handler-case
        (let ((result (funcall (tool-function tool) arguments)))
          (tool-result (if (stringp result) result (princ-to-string result))))
      (error (condition)
        (log-message "tool ~A failed: ~A" name condition)
        (tool-result (princ-to-string condition) t)))))
```

Because the `osascript-error` condition from the previous chapter has a `:report` function, `princ-to-string` turns it into the real AppleScript diagnostic, for example `osascript failed (exit 1): ... Not authorized to send Apple events to Mail`. The model then gets a message it can pass on to the user.

`dispatch` maps a method name to its handler, and `handle-message` processes one parsed message:

```lisp
(defun dispatch (method params)
  (cond ((equal method "initialize") (handle-initialize params))
        ((equal method "ping") (obj))
        ((equal method "tools/list") (handle-tools-list))
        ((equal method "tools/call") (handle-tools-call params))
        (t (rpc-error -32601 "Method not found: ~A" method))))

(defun handle-message (message)
  "Process one parsed JSON-RPC MESSAGE. Return the response object, or NIL
for notifications, which never get a response."
  (multiple-value-bind (id requestp)
      (if (hash-table-p message) (gethash "id" message) (values nil nil))
    (let ((method (json-get message "method"))
          (params (json-get message "params")))
      (cond ((not (stringp method))
             (and requestp (error-response id -32600 "Invalid request")))
            ((not requestp)
             (log-message "notification ~A" method)
             nil)
            (t
             (handler-case (success-response id (dispatch method params))
               (json-rpc-error (condition)
                 (error-response id (json-rpc-error-code condition)
                                 (json-rpc-error-message condition)))
               (error (condition)
                 (error-response id -32603 (princ-to-string condition)))))))))
```

We use the second value of `gethash` to tell a request from a notification. Testing the `id` value itself would fail for a request whose `id` is `0` or `false`; the presence of the key is what counts.

### The Main Loop

```lisp
(defun handle-line (line)
  (let ((message (handler-case (jzon:parse line)
                   (error () (return-from handle-line
                               (error-response 'null -32700 "Parse error"))))))
    (handle-message message)))

(defun start (&key (input *standard-input*) (output *standard-output*))
  "Read JSON-RPC messages from INPUT, one per line, until end of file."
  (log-message "~A ~A started with ~D tools"
               *server-name* *server-version* (hash-table-count *tools*))
  ;; Anything printed to *standard-output* would corrupt the protocol
  ;; stream, so point it at stderr while tools run.
  (let ((*standard-output* *error-output*))
    (loop for line = (read-line input nil)
          while line
          unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
            do (let ((response (handle-line line)))
                 (when response
                   ;; jzon escapes newlines inside strings, so each
                   ;; message fits on one line as the transport requires.
                   (write-line (jzon:stringify response) output)
                   (finish-output output)))))
  (log-message "stdin closed, exiting"))
```

`start` grabs the real stdout as `output` before it binds `*standard-output*` to stderr. After that, a stray `print` in any tool lands in the log and cannot break the protocol. `finish-output` after each message matters: without it SBCL buffers the response and the client waits forever. When the client closes stdin, `read-line` returns `nil` and the server exits.

## The Tools: mac-tools.lisp

With `deftool` in place, each tool takes a few lines:

```lisp
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
```

Write tool descriptions for the model, not for a human reader. The model chooses tools from these strings, so say what the tool does and what it needs.

`todays-events` removes empty strings because `get-todays-events` splits an empty AppleScript result into a list holding one empty string. `read-recent-imessages` adds a range check on top of the type check so a model can't ask for the whole message history at once.

I left `safari-login` out of the tool list. It takes a password, and a password should never pass through an LLM conversation.

## Starting the Server: run-server.sh

Clients start the server with a shell script:

```bash
#!/bin/sh
# Start the MCP server on stdio. Stdout carries only JSON-RPC messages.

DIR="$(cd "$(dirname "$0")" && pwd)"

exec sbcl --noinform --non-interactive \
  --eval "(asdf:load-asd \"$DIR/mcp-common-lisp.asd\")" \
  --eval "(ql:quickload :mcp-common-lisp :silent t)" \
  --eval "(mcp-server:start)"
```

The script assumes your `~/.sbclrc` loads Quicklisp, as the Quicklisp installer sets up. `asdf:load-asd` tells ASDF where the system lives, so the script works from any directory. The `:silent t` option keeps Quicklisp's progress messages off stdout, and `--noinform` drops the SBCL banner.

Your `~/.sbclrc` must not print to stdout either. I once had `(ql:quickload :cffi)` in mine, and its `To load "cffi":` message reached the client before the server started. A client reads such a line as a broken JSON message. To check your setup, run:

```bash
echo | sbcl --noinform --non-interactive
```

This command should print nothing.

## Testing Without an LLM

You don't need an LLM client to test an MCP server. Since the protocol is text lines on stdin and stdout, a shell here-document can play the client. The script `test-session.sh` sends a session that exercises each code path without touching a macOS app:

```bash
#!/bin/sh
# Send a short MCP session to the server and print its replies.
# These calls do not touch any macOS app.

DIR="$(cd "$(dirname "$0")" && pwd)"

"$DIR/run-server.sh" 2>/dev/null <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"read_recent_imessages","arguments":{"limit":"5; DROP TABLE message"}}}
{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"add_reminder","arguments":{}}}
{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"no_such_tool","arguments":{}}}
{"jsonrpc":"2.0","id":6,"method":"ping"}
not json
EOF
```

The output, with the long `tools/list` response cut short:

```
$ ./test-session.sh
{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-06-18","capabilities":{"tools":{"listChanged":false}},"serverInfo":{"name":"common-lisp-macos","version":"0.1.0"}}}
{"jsonrpc":"2.0","id":2,"result":{"tools":[{"name":"add_reminder","description":"Create a new reminder in the macOS Reminders app.","inputSchema":{"type":"object","properties":{"task_name":{"type":"string","description":"Text of the new reminder"}},"required":["task_name"]}},{"name":"mail_unread_count", ...
{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"Argument limit must be of type integer"}],"isError":true}}
{"jsonrpc":"2.0","id":4,"result":{"content":[{"type":"text","text":"Missing required argument: task_name"}],"isError":true}}
{"jsonrpc":"2.0","id":5,"error":{"code":-32602,"message":"Unknown tool: no_such_tool"}}
{"jsonrpc":"2.0","id":6,"result":{}}
{"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"Parse error"}}
```

Check each line against the protocol rules:

- Seven requests produced seven responses. The `notifications/initialized` line produced none.
- The server echoed the client's protocol version `2025-06-18`.
- The injection attempt and the missing argument came back as tool results with `isError` true.
- The unknown tool came back as a JSON-RPC error with code -32602.
- The line `not json` got a parse error with a `null` id, since the server could not read an id from it.

To test a real tool, send one `tools/call` line. Here I keep stderr so the log lines show:

```
$ echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"todays_events","arguments":{}}}' | ./run-server.sh
[mcp] common-lisp-macos 0.1.0 started with 5 tools
[mcp] tools/call todays_events
{"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"Mark sutures"}],"isError":false}}
[mcp] stdin closed, exiting
```

The first time a tool talks to an app, macOS asks whether the process may control that app. The previous chapter covers these Automation and Full Disk Access settings.

For interactive testing, the MCP project provides a web-based inspector that lists your tools and lets you call them from a form:

```bash
npx @modelcontextprotocol/inspector /path/to/src/MCP_common_lisp/run-server.sh
```

## Using the Server from Claude Code and Claude Desktop

To add the server to Claude Code, give it a name and the command that starts it:

```bash
claude mcp add macos-lisp -- /path/to/src/MCP_common_lisp/run-server.sh
```

For Claude Desktop, add an entry to `~/Library/Application Support/Claude/claude_desktop_config.json` and restart the app:

```json
{
  "mcpServers": {
    "macos-lisp": {
      "command": "/path/to/src/MCP_common_lisp/run-server.sh"
    }
  }
}
```

Replace `/path/to` with the location of your copy of the book's source code. You can now ask questions like "What's on my calendar today?" or "Remind me to renew my passport", and the model calls the matching tool.

macOS applies its privacy settings to the app that starts the server, so you may need to grant Automation and Full Disk Access to Claude Desktop or to your terminal, depending on the client.

The `send_imessage` tool sends real messages. Most MCP clients ask you to approve each tool call by default; keep that setting on for this tool.

## Adding Your Own Tools

To add a tool, write a `deftool` form in `mac-tools.lisp` or in a new file listed in the `.asd` file. Here is a tool that gives the model the current date and time, which LLMs do not know on their own:

```lisp
(deftool current-time ()
  "Return the current local date and time."
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))
```

Since `*tools*` is a hash table that `tools/list` reads on each call, a tool you define in a live Lisp image appears the next time a client lists the tools. With a REPL attached to a running server, you can add and fix tools without a restart.

## Wrap Up

An MCP server is a loop that reads JSON-RPC lines from stdin and writes JSON-RPC lines to stdout. In Common Lisp the protocol code fits in one file, and a short macro turns any Lisp function into a tool that every MCP client can call.

The points to remember:

- Keep stdout clean. Send logs to stderr, load systems with `:silent t`, and keep `~/.sbclrc` quiet.
- Call `finish-output` after each response.
- Never answer a notification.
- Report tool failures as results with `isError` true, and protocol failures as JSON-RPC errors.
- Check the type of every argument. The model writes them, and the values may end up inside SQL or AppleScript.

The same pattern works for any Lisp code in this book. Good next steps would be tools that run SPARQL queries against the RDF store from the Knowledge Graph chapters, or tools that search a document collection with the RAG code from Part 7.
