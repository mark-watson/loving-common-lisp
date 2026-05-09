;;; webkit-cl.lisp — High-level API for webkit-cl
;;;
;;; Provides the user-facing functions for creating and managing
;;; WebKit-based GUI applications from Common Lisp.

(in-package #:webkit-cl)

;;; ── App Structure ──────────────────────────────────────────────

(defvar *current-app* nil
  "The currently active webkit-cl app handle.")

(defstruct (app (:constructor %make-app-internal))
  "A webkit-cl application instance."
  (handle (cffi:null-pointer) :type cffi:foreign-pointer)
  (title "webkit-cl" :type string)
  (width 800 :type integer)
  (height 600 :type integer))

;;; ── App Lifecycle ──────────────────────────────────────────────

(defun %disable-fp-traps ()
  "Disable SBCL floating-point traps. Required because macOS Cocoa/AppKit
   triggers FP operations that conflict with SBCL's default trap settings."
  #+sbcl (sb-int:set-floating-point-modes :traps nil))

(defun make-app (&key (title "webkit-cl") (width 800) (height 600))
  "Create a new webkit-cl application (does not start the event loop).

   Returns an APP struct. Call APP-RUN to show the window and enter
   the event loop."
  (%disable-fp-traps)
  (let* ((handle (%wkcl-create title width height))
         (app (%make-app-internal :handle handle
                                  :title title
                                  :width width
                                  :height height)))
    ;; Install the bridge callback
    (%wkcl-set-bridge-callback handle
                                (cffi:callback bridge-dispatch)
                                (cffi:null-pointer))
    app))

(defun app-run (app)
  "Start the application event loop (blocks until the window is closed).
   Sets *CURRENT-APP* for the duration."
  (%disable-fp-traps)
  (let ((*current-app* app))
    (%wkcl-run (app-handle app))))

(defun app-quit (&optional (app *current-app*))
  "Request the application to quit."
  (when app
    (%wkcl-quit (app-handle app))))

(defun app-destroy (app)
  "Destroy the application and free native resources."
  (when app
    (%wkcl-destroy (app-handle app))
    (setf (app-handle app) (cffi:null-pointer))))

;;; ── Content Loading ────────────────────────────────────────────

(defun load-html (html &optional (app *current-app*))
  "Load inline HTML content into the WebView.

   Example:
     (load-html \"<h1>Hello!</h1>\")"
  (when app
    (%wkcl-load-html (app-handle app) html)))

(defun load-url (url &optional (app *current-app*))
  "Navigate the WebView to a URL.

   Example:
     (load-url \"https://example.com\")"
  (when app
    (%wkcl-load-url (app-handle app) url)))

(defun load-file (path &optional (app *current-app*))
  "Load a local HTML file into the WebView.
   PATH can be absolute or relative to the current directory.

   Example:
     (load-file \"ui/index.html\")"
  (when app
    (%wkcl-load-file (app-handle app) (namestring (truename path)))))

;;; ── JavaScript ─────────────────────────────────────────────────

(defun eval-js (js &optional (app *current-app*))
  "Evaluate JavaScript in the WebView (fire-and-forget).

   Example:
     (eval-js \"document.title = 'Updated'\")"
  (when app
    (%wkcl-eval-js (app-handle app) js)))

;;; ── Window Management ──────────────────────────────────────────

(defun set-title (title &optional (app *current-app*))
  "Change the window title."
  (when app
    (%wkcl-set-title (app-handle app) title)))

(defun set-size (width height &optional (app *current-app*))
  "Resize the window."
  (when app
    (%wkcl-set-size (app-handle app) width height)))

(defun set-resizable (resizable-p &optional (app *current-app*))
  "Set whether the window is resizable."
  (when app
    (%wkcl-set-resizable (app-handle app) (if resizable-p 1 0))))

;;; ── Convenience Macro ──────────────────────────────────────────

(defmacro with-app ((&key (title "webkit-cl") (width 800) (height 600))
                    &body body)
  "Create a webkit-cl app, execute BODY (which should load content
   and register handlers), then run the event loop.

   The app is automatically destroyed when the window is closed.

   Example:
     (with-app (:title \"My App\" :width 960 :height 640)
       (register-handler \"ping\"
         (lambda (payload)
           (declare (ignore payload))
           (json-response :message \"pong\")))
       (load-html \"<button onclick=\\\"test()\\\">Click</button>\"))"
  (let ((app-var (gensym "APP")))
    `(let* ((,app-var (make-app :title ,title :width ,width :height ,height))
            (*current-app* ,app-var))
       (unwind-protect
            (progn ,@body
                   (app-run ,app-var))
         (app-destroy ,app-var)))))
