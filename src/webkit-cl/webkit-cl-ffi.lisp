;;; webkit-cl-ffi.lisp — CFFI bindings to libwebkit_cl.dylib
;;;
;;; Low-level foreign function definitions matching webkit_cl.h.
;;; Users should prefer the high-level API in webkit-cl.lisp.

(in-package #:webkit-cl)

;;; ── Library Loading ────────────────────────────────────────────

(cffi:define-foreign-library libwebkit-cl
  (:darwin (:or
            ;; Look in the same directory as the .lisp files first
            "libwebkit_cl.dylib"
            ;; Then try the standard search paths
            (:default "libwebkit_cl")))
  (t (:default "libwebkit_cl")))

(defparameter *source-directory*
  ;; Read at compile time, so this is the directory of the .lisp source (where
  ;; `make` puts the dylib). At load time *LOAD-PATHNAME* would instead be the
  ;; compiled .fasl in ASDF's output cache, where the dylib is never found.
  (make-pathname :name nil :type nil :version nil
                 :defaults #.(or *compile-file-truename* *load-truename*))
  "Directory containing the webkit-cl sources and libwebkit_cl.dylib.")

(defun load-native-library ()
  "Load the webkit-cl native library. Searches the source directory first."
  ;; Push the source directory onto the search path
  (pushnew *source-directory* cffi:*foreign-library-directories* :test #'equal)
  (cffi:use-foreign-library libwebkit-cl))

(load-native-library)

;;; ── Type Definitions ───────────────────────────────────────────

;; wkcl_app_t is an opaque pointer (void*)
(cffi:defctype wkcl-app-t :pointer)

;; Bridge callback: const char* (*)(const char*, const char*, void*)
(cffi:defcallback bridge-dispatch :pointer
    ((command :string) (payload :string) (userdata :pointer))
  "Master bridge callback that dispatches to registered Lisp handlers."
  (declare (ignore userdata))
  (let ((result (dispatch-bridge-command command payload)))
    (if result
        (cffi:foreign-string-alloc result)
        (cffi:null-pointer))))

;;; ── Foreign Function Definitions ───────────────────────────────

;; Lifecycle
(cffi:defcfun ("wkcl_create" %wkcl-create) wkcl-app-t
  (title :string)
  (width :int)
  (height :int))

(cffi:defcfun ("wkcl_run" %wkcl-run) :void
  (app wkcl-app-t))

(cffi:defcfun ("wkcl_quit" %wkcl-quit) :void
  (app wkcl-app-t))

(cffi:defcfun ("wkcl_destroy" %wkcl-destroy) :void
  (app wkcl-app-t))

;; Content loading
(cffi:defcfun ("wkcl_load_html" %wkcl-load-html) :void
  (app wkcl-app-t)
  (html :string))

(cffi:defcfun ("wkcl_load_url" %wkcl-load-url) :void
  (app wkcl-app-t)
  (url :string))

(cffi:defcfun ("wkcl_load_file" %wkcl-load-file) :void
  (app wkcl-app-t)
  (path :string))

;; JavaScript
(cffi:defcfun ("wkcl_eval_js" %wkcl-eval-js) :void
  (app wkcl-app-t)
  (js :string))

;; Bridge
(cffi:defcfun ("wkcl_set_bridge_callback" %wkcl-set-bridge-callback) :void
  (app wkcl-app-t)
  (callback :pointer)
  (userdata :pointer))

;; Window management
(cffi:defcfun ("wkcl_set_title" %wkcl-set-title) :void
  (app wkcl-app-t)
  (title :string))

(cffi:defcfun ("wkcl_set_size" %wkcl-set-size) :void
  (app wkcl-app-t)
  (width :int)
  (height :int))

(cffi:defcfun ("wkcl_set_resizable" %wkcl-set-resizable) :void
  (app wkcl-app-t)
  (resizable :int))
