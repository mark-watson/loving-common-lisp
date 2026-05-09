# WebKit Applications - macOS Only

In this chapter we build native macOS desktop applications using Common Lisp and WebKit. The **webkit-cl** library lets you create windows with embedded WKWebView panels, load HTML/CSS/JavaScript UIs, and communicate between Lisp and JavaScript through a bidirectional bridge. This approach gives you the full power of Common Lisp for application logic while using modern web technologies for the user interface.

**Note: This library works only on macOS. It requires SBCL, CFFI, and cl-json.**

## Architecture Overview

The webkit-cl framework is organized in four layers:

1. **Objective-C shim** (`webkit_cl.m`) — Bridges macOS Cocoa and WebKit APIs to a flat C interface
2. **CFFI bindings** (`webkit-cl-ffi.lisp`) — Exposes the C functions to Common Lisp
3. **Bridge** (`bridge.lisp`) — Manages JS ↔ Lisp command dispatch and JSON serialization
4. **High-level API** (`webkit-cl.lisp`) — Idiomatic Lisp functions: `with-app`, `load-html`, `register-handler`, etc.

When JavaScript calls `window.webkit_cl.invoke("command", payload)`, the message travels through WKWebView's script message handler into the C shim, through CFFI into Lisp, where a registered handler processes it and returns a JSON response. The response flows back to JavaScript via a Promise.

## Prerequisites and Building

You need macOS (Apple Silicon or Intel), SBCL, and Quicklisp with `cffi` and `cl-json` installed. Build the native library with:

{lang="bash",linenos=off}
~~~~~~~~
cd src/webkit-cl
make
~~~~~~~~

This compiles the Objective-C shim into `libwebkit_cl.dylib`:

{linenos=off}
~~~~~~~~
clang -fobjc-arc -fPIC -O2 -Wall -framework Cocoa -framework WebKit \
  -dynamiclib -install_name @rpath/libwebkit_cl.dylib \
  -o libwebkit_cl.dylib webkit_cl.m
~~~~~~~~

## Project Structure

The ASDF system definition ties the components together:

{lang="lisp",linenos=off}
~~~~~~~~
;;; webkit-cl.asd — ASDF system definition for webkit-cl

(asdf:defsystem #:webkit-cl
  :description "Lightweight WebKit GUI apps for Common Lisp (macOS)"
  :author "Mark Watson"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on (#:cffi #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "webkit-cl-ffi")
               (:file "bridge")
               (:file "webkit-cl")))
~~~~~~~~

The package exports the public API:

{lang="lisp",linenos=off}
~~~~~~~~
(defpackage #:webkit-cl
  (:use #:cl)
  (:export
   ;; App lifecycle
   #:with-app
   #:make-app
   #:app-run
   #:app-quit
   #:app-destroy
   ;; Content loading
   #:load-html
   #:load-url
   #:load-file
   ;; JavaScript evaluation
   #:eval-js
   ;; Bridge
   #:register-handler
   #:unregister-handler
   ;; Window management
   #:set-title
   #:set-size
   #:set-resizable
   ;; App accessors
   #:app-handle
   #:*current-app*))
~~~~~~~~

## The C Shim

The C header (`webkit_cl.h`) defines a minimal interface. All functions take an opaque `wkcl_app_t` handle:

{lang="c",linenos=off}
~~~~~~~~
typedef void* wkcl_app_t;

/* Callback for bridge invocations from JavaScript */
typedef const char* (*wkcl_bridge_callback_t)(const char* command,
                                              const char* payload,
                                              void* userdata);

/* Lifecycle */
wkcl_app_t wkcl_create(const char* title, int width, int height);
void wkcl_run(wkcl_app_t app);
void wkcl_quit(wkcl_app_t app);
void wkcl_destroy(wkcl_app_t app);

/* Content loading */
void wkcl_load_html(wkcl_app_t app, const char* html);
void wkcl_load_url(wkcl_app_t app, const char* url);
void wkcl_load_file(wkcl_app_t app, const char* path);

/* JavaScript & Bridge */
void wkcl_eval_js(wkcl_app_t app, const char* js);
void wkcl_set_bridge_callback(wkcl_app_t app,
                               wkcl_bridge_callback_t callback,
                               void* userdata);

/* Window management */
void wkcl_set_title(wkcl_app_t app, const char* title);
void wkcl_set_size(wkcl_app_t app, int width, int height);
void wkcl_set_resizable(wkcl_app_t app, int resizable);
~~~~~~~~

The Objective-C implementation (`webkit_cl.m`) creates an `NSApplication` with a `WKWebView` inside an `NSWindow`. The bridge works by injecting a JavaScript snippet at document start that defines `window.webkit_cl.invoke()`. This function posts messages to a `WKScriptMessageHandler`, which routes them to the registered C callback. The callback returns a `malloc`'d JSON string that is sent back to JavaScript via `evaluateJavaScript:`.

Here is the bridge JavaScript that gets injected into every page:

{lang="javascript",linenos=off}
~~~~~~~~
window.webkit_cl = {
    _callbackId: 0,
    _callbacks: {},
    invoke: function(command, payload) {
        return new Promise(function(resolve, reject) {
            var id = String(++window.webkit_cl._callbackId);
            window.webkit_cl._callbacks[id] = {
                resolve: resolve, reject: reject
            };
            var msg = {
                command: command,
                payload: JSON.stringify(payload || {}),
                callbackId: id
            };
            window.webkit.messageHandlers.wkcl_bridge
                .postMessage(msg);
        });
    },
    _resolveCallback: function(id, result) {
        var cb = window.webkit_cl._callbacks[id];
        if (cb) {
            cb.resolve(result);
            delete window.webkit_cl._callbacks[id];
        }
    }
};
~~~~~~~~

Each `invoke()` call returns a Promise. The Objective-C handler calls the C callback, gets a JSON result, and resolves the Promise by evaluating `window.webkit_cl._resolveCallback(id, result)`.

## CFFI Bindings

The FFI layer maps the C API to Common Lisp. The library is loaded from the same directory as the source files:

{lang="lisp",linenos=off}
~~~~~~~~
(cffi:define-foreign-library libwebkit-cl
  (:darwin (:or "libwebkit_cl.dylib"
                (:default "libwebkit_cl")))
  (t (:default "libwebkit_cl")))

(defun load-native-library ()
  "Load the webkit-cl native library."
  (let* ((this-file (or *compile-file-pathname* *load-pathname*))
         (lib-dir (when this-file
                    (namestring
                     (make-pathname
                      :directory (pathname-directory this-file))))))
    (when lib-dir
      (pushnew (pathname lib-dir)
               cffi:*foreign-library-directories*
               :test #'equal))
    (cffi:use-foreign-library libwebkit-cl)))

(load-native-library)
~~~~~~~~

The bridge callback is defined with `cffi:defcallback`. It dispatches to the Lisp-side handler registry:

{lang="lisp",linenos=off}
~~~~~~~~
(cffi:defcallback bridge-dispatch :pointer
    ((command :string) (payload :string) (userdata :pointer))
  "Master bridge callback that dispatches to registered Lisp handlers."
  (declare (ignore userdata))
  (let ((result (dispatch-bridge-command command payload)))
    (if result
        (cffi:foreign-string-alloc result)
        (cffi:null-pointer))))
~~~~~~~~

Note that the callback returns a `cffi:foreign-string-alloc`'d pointer — the C side will `free()` it after use.

## The Bridge: JS ↔ Lisp Communication

The bridge module maintains a hash table of named command handlers:

{lang="lisp",linenos=off}
~~~~~~~~
(defvar *bridge-handlers* (make-hash-table :test 'equal)
  "Hash table mapping command names (strings) to handler functions.")

(defun register-handler (command handler-fn)
  "Register a bridge handler for COMMAND.
   HANDLER-FN takes one argument: the parsed payload (alist from cl-json).
   It should return a JSON string to send back to JavaScript."
  (setf (gethash command *bridge-handlers*) handler-fn)
  command)

(defun unregister-handler (command)
  "Remove the bridge handler for COMMAND."
  (remhash command *bridge-handlers*)
  command)
~~~~~~~~

When JavaScript calls `window.webkit_cl.invoke("greet", {name: "World"})`, the dispatch function looks up the handler by command name, parses the JSON payload with `cl-json`, calls the handler, and returns the result:

{lang="lisp",linenos=off}
~~~~~~~~
(defun dispatch-bridge-command (command payload-json)
  "Dispatch a bridge command to the registered handler."
  (let ((handler (gethash command *bridge-handlers*)))
    (if handler
        (handler-case
            (let* ((payload (handler-case
                                (json:decode-json-from-string payload-json)
                              (error () nil)))
                   (result (funcall handler payload)))
              (if result result "null"))
          (error (e)
            (format nil "{\"error\": \"~a\"}"
                    (substitute-json-chars (format nil "~a" e)))))
        (format nil "{\"error\": \"unknown command: ~a\"}"
                (substitute-json-chars command)))))
~~~~~~~~

The double `handler-case` nesting is deliberate: one catches JSON parse errors (which are non-fatal — the handler receives `nil`), the other catches handler execution errors and returns them as JSON error objects to JavaScript.

## High-Level API

The main API provides a struct-based app object and a convenience macro:

{lang="lisp",linenos=off}
~~~~~~~~
(defvar *current-app* nil
  "The currently active webkit-cl app handle.")

(defstruct (app (:constructor %make-app-internal))
  "A webkit-cl application instance."
  (handle (cffi:null-pointer) :type cffi:foreign-pointer)
  (title "webkit-cl" :type string)
  (width 800 :type integer)
  (height 600 :type integer))
~~~~~~~~

The `make-app` function creates the native window and installs the bridge callback:

{lang="lisp",linenos=off}
~~~~~~~~
(defun make-app (&key (title "webkit-cl") (width 800) (height 600))
  "Create a new webkit-cl application (does not start the event loop)."
  (%disable-fp-traps)
  (let* ((handle (%wkcl-create title width height))
         (app (%make-app-internal :handle handle
                                   :title title
                                   :width width
                                   :height height)))
    (%wkcl-set-bridge-callback handle
                                (cffi:callback bridge-dispatch)
                                (cffi:null-pointer))
    app))
~~~~~~~~

The `%disable-fp-traps` call is essential — macOS Cocoa/AppKit triggers floating-point operations that conflict with SBCL's default trap settings.

The `with-app` macro is the recommended entry point. It creates the app, executes the body (where you register handlers and load content), runs the event loop, and cleans up:

{lang="lisp",linenos=off}
~~~~~~~~
(defmacro with-app ((&key (title "webkit-cl") (width 800) (height 600))
                    &body body)
  "Create a webkit-cl app, execute BODY, then run the event loop.
   The app is automatically destroyed when the window is closed."
  (let ((app-var (gensym "APP")))
    `(let* ((,app-var (make-app :title ,title
                                :width ,width :height ,height))
            (*current-app* ,app-var))
       (unwind-protect
            (progn ,@body
                   (app-run ,app-var))
         (app-destroy ,app-var)))))
~~~~~~~~

Content loading and JavaScript evaluation are thin wrappers around the C API:

{lang="lisp",linenos=off}
~~~~~~~~
(defun load-html (html &optional (app *current-app*))
  "Load inline HTML content into the WebView."
  (when app
    (%wkcl-load-html (app-handle app) html)))

(defun load-url (url &optional (app *current-app*))
  "Navigate the WebView to a URL."
  (when app
    (%wkcl-load-url (app-handle app) url)))

(defun load-file (path &optional (app *current-app*))
  "Load a local HTML file into the WebView."
  (when app
    (%wkcl-load-file (app-handle app) (namestring (truename path)))))

(defun eval-js (js &optional (app *current-app*))
  "Evaluate JavaScript in the WebView (fire-and-forget)."
  (when app
    (%wkcl-eval-js (app-handle app) js)))
~~~~~~~~

## Example 1: Hello World

The simplest webkit-cl app loads inline HTML into a native window:

{lang="lisp",linenos=off}
~~~~~~~~
(require :asdf)
(push (make-pathname :directory (pathname-directory *load-pathname*))
      asdf:*central-registry*)
(asdf:load-system :webkit-cl)

(webkit-cl:with-app (:title "Hello webkit-cl" :width 600 :height 400)
  (webkit-cl:load-html
   "<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, system-ui, sans-serif;
    background: linear-gradient(135deg, #0f0c29 0%, #302b63 50%, #24243e 100%);
    color: #e0e0e0;
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100vh;
  }
  .card {
    text-align: center;
    background: rgba(255,255,255,0.05);
    backdrop-filter: blur(20px);
    border: 1px solid rgba(255,255,255,0.1);
    border-radius: 24px;
    padding: 48px 64px;
    box-shadow: 0 8px 32px rgba(0,0,0,0.3);
  }
  h1 {
    font-size: 2.5em;
    background: linear-gradient(90deg, #a78bfa, #60a5fa, #34d399);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    margin-bottom: 12px;
  }
  p { font-size: 1.1em; color: rgba(255,255,255,0.6); }
  .badge {
    display: inline-block;
    margin-top: 20px;
    padding: 6px 16px;
    font-size: 0.85em;
    background: rgba(167,139,250,0.15);
    border: 1px solid rgba(167,139,250,0.3);
    border-radius: 999px;
    color: #a78bfa;
  }
</style>
</head>
<body>
  <div class='card'>
    <h1>Hello, webkit-cl!</h1>
    <p>A native macOS window powered by Common Lisp<br>
       and WebKit (WKWebView).</p>
    <span class='badge'>SBCL + Cocoa + WebKit</span>
  </div>
</body>
</html>"))
~~~~~~~~

Run it with:

{lang="bash",linenos=off}
~~~~~~~~
sbcl --load examples/hello-world.lisp
~~~~~~~~

A native macOS window appears with a gradient background, glassmorphism card, and gradient text — all rendered by the system WebKit engine.

## Example 2: Counter App with Bridge

This example demonstrates bidirectional communication. Lisp manages the application state (a counter), and JavaScript provides the UI:

{lang="lisp",linenos=off}
~~~~~~~~
(require :asdf)
(push (make-pathname :directory (pathname-directory
                                  (make-pathname
                                    :directory (butlast
                                      (pathname-directory *load-pathname*)))))
      asdf:*central-registry*)
(asdf:load-system :webkit-cl)

;;; Application State
(defvar *counter* 0)

;;; Bridge Handlers
(webkit-cl:register-handler "increment"
  (lambda (payload)
    (declare (ignore payload))
    (incf *counter*)
    (format nil "{\"count\": ~d}" *counter*)))

(webkit-cl:register-handler "decrement"
  (lambda (payload)
    (declare (ignore payload))
    (decf *counter*)
    (format nil "{\"count\": ~d}" *counter*)))

(webkit-cl:register-handler "reset"
  (lambda (payload)
    (declare (ignore payload))
    (setf *counter* 0)
    (format nil "{\"count\": ~d}" *counter*)))

(webkit-cl:register-handler "get-system-info"
  (lambda (payload)
    (declare (ignore payload))
    (format nil "{\"lisp\": \"~a\", \"version\": \"~a\", \"machine\": \"~a\"}"
            (lisp-implementation-type)
            (lisp-implementation-version)
            (machine-type))))
~~~~~~~~

Each handler receives a parsed JSON payload (an alist from `cl-json`) and returns a JSON string. The JavaScript side calls these handlers through the bridge:

{lang="javascript",linenos=off}
~~~~~~~~
async function increment() {
  const result = await window.webkit_cl.invoke('increment', {});
  updateDisplay(result.count);
}

// On startup, query the Lisp runtime
const sysInfo = await window.webkit_cl.invoke('get-system-info', {});
info.innerHTML = 'Powered by ' + sysInfo.lisp + ' ' + sysInfo.version;
~~~~~~~~

The counter value lives entirely in Lisp — JavaScript only renders it. This pattern cleanly separates application logic (Lisp) from presentation (HTML/CSS/JS).

Run it with:

{lang="bash",linenos=off}
~~~~~~~~
sbcl --load examples/counter-app.lisp
~~~~~~~~

## Example 3: Markdown File Viewer

The most complete example demonstrates filesystem access through the bridge. Two handlers let JavaScript list and read files:

{lang="lisp",linenos=off}
~~~~~~~~
(webkit-cl:register-handler "read-file"
  (lambda (payload)
    (let ((path (cdr (assoc :path payload))))
      (if (and path (probe-file path))
          (let ((content (with-open-file (s path :direction :input)
                           (let ((data (make-string (file-length s))))
                             (read-sequence data s)
                             data))))
            (format nil "{\"content\": ~a, \"path\": ~a}"
                    (json:encode-json-to-string content)
                    (json:encode-json-to-string path)))
          (format nil "{\"error\": \"File not found: ~a\"}"
                  (or path "nil"))))))

(webkit-cl:register-handler "list-files"
  (lambda (payload)
    (let* ((dir (or (cdr (assoc :directory payload)) "."))
           (pattern (merge-pathnames "*.md" (pathname dir)))
           (files (directory pattern)))
      (format nil "{\"files\": [~{~a~^, ~}]}"
              (mapcar (lambda (f)
                        (json:encode-json-to-string (namestring f)))
                      files)))))
~~~~~~~~

The `read-file` handler uses `json:encode-json-to-string` to safely escape file contents for JSON embedding. The `list-files` handler uses `directory` with a wildcard pattern and the format directive `~{~a~^, ~}` to build a JSON array from the results.

The UI is a split-pane layout with a file sidebar and content area. JavaScript calls the bridge on startup:

{lang="javascript",linenos=off}
~~~~~~~~
async function loadFileList() {
  const result = await window.webkit_cl.invoke('list-files',
                                                { directory: '.' });
  if (result.files && result.files.length > 0) {
    fileList.innerHTML = result.files.map(f =>
      '<div class="file-item" onclick="loadFile(\'' + f + '\')">' +
      '<div class="name">' + basename(f) + '</div>' +
      '</div>'
    ).join('');
  }
}

async function loadFile(path) {
  const result = await window.webkit_cl.invoke('read-file',
                                                { path: path });
  if (result.content) {
    content.innerHTML = '<pre>' + escapeHtml(result.content) + '</pre>';
  }
}
~~~~~~~~

Run it with:

{lang="bash",linenos=off}
~~~~~~~~
sbcl --load examples/markdown-viewer.lisp
~~~~~~~~

This opens a native window with a dark sidebar listing `.md` files from the current directory. Clicking a file reads its content via the Lisp bridge and displays it in a styled code panel.

## API Reference Summary

The webkit-cl public API:

| Function | Description |
|---|---|
| `(with-app (&key title width height) &body body)` | Create app, run body, enter event loop, auto-cleanup |
| `(load-html html)` | Load inline HTML string |
| `(load-url url)` | Navigate to a URL |
| `(load-file path)` | Load a local HTML file |
| `(eval-js js-string)` | Evaluate JavaScript (fire-and-forget) |
| `(register-handler name fn)` | Register a bridge command handler |
| `(unregister-handler name)` | Remove a bridge command handler |
| `(set-title title)` | Change window title |
| `(set-size width height)` | Resize window |
| `(set-resizable flag)` | Toggle window resizability |

From JavaScript, call Lisp handlers with:

{lang="javascript",linenos=off}
~~~~~~~~
const result = await window.webkit_cl.invoke("command-name", {key: "value"});
~~~~~~~~

## Key Takeaways

1. **CFFI + Objective-C** — Common Lisp can drive native macOS frameworks through a thin C shim compiled as a dynamic library
2. **WKWebView** — The system WebKit engine provides a modern, full-featured rendering surface without bundling a browser
3. **Bridge pattern** — Named command handlers with JSON message passing cleanly separate Lisp logic from JavaScript UI
4. **`unwind-protect`** — The `with-app` macro ensures native resources are freed even if an error occurs
5. **Floating-point traps** — SBCL's default FP trap settings conflict with Cocoa; `%disable-fp-traps` is essential

This framework demonstrates that Common Lisp can build polished desktop applications. The web rendering layer handles the visual complexity while Lisp provides the computational backbone — a productive division of labor for tools, dashboards, and data viewers.
