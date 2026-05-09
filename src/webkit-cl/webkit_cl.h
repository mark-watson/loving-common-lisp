/*
 * webkit_cl.h — C API for webkit-cl
 *
 * A minimal C interface to macOS Cocoa + WKWebView, designed to be
 * called from Common Lisp via CFFI. Inspired by zero-native's system
 * WebView approach but stripped down for simplicity.
 *
 * Thread safety: All functions must be called from the main thread.
 * The run loop (wkcl_run) blocks until the app quits.
 */

#ifndef WEBKIT_CL_H
#define WEBKIT_CL_H

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque handle to a webkit-cl application instance */
typedef void* wkcl_app_t;

/*
 * Callback type for bridge invocations from JavaScript.
 *
 * When JS calls window.webkit_cl.invoke(command, payload),
 * this callback fires with:
 *   - command:  the command name (UTF-8 string)
 *   - payload:  JSON string of the payload
 *   - userdata: the pointer passed during handler registration
 *
 * The callback must return a malloc'd JSON string (the bridge will free it),
 * or NULL to indicate no response / error.
 */
typedef const char* (*wkcl_bridge_callback_t)(const char* command,
                                               const char* payload,
                                               void* userdata);

/* ── Lifecycle ───────────────────────────────────────────────── */

/*
 * Create a new webkit-cl application.
 * title:  window title (UTF-8)
 * width:  initial window width in points
 * height: initial window height in points
 * Returns an opaque handle, or NULL on failure.
 */
wkcl_app_t wkcl_create(const char* title, int width, int height);

/*
 * Start the application run loop (blocks until quit).
 * This initializes NSApplication if needed, shows the window,
 * and enters the Cocoa event loop.
 */
void wkcl_run(wkcl_app_t app);

/*
 * Request the application to quit.
 * Can be called from a bridge handler or signal handler.
 */
void wkcl_quit(wkcl_app_t app);

/*
 * Destroy the application and free resources.
 * Call after wkcl_run returns.
 */
void wkcl_destroy(wkcl_app_t app);

/* ── Content Loading ─────────────────────────────────────────── */

/*
 * Load inline HTML content into the WebView.
 * html: UTF-8 HTML string
 */
void wkcl_load_html(wkcl_app_t app, const char* html);

/*
 * Navigate the WebView to a URL.
 * url: UTF-8 URL string (http://, https://, file://)
 */
void wkcl_load_url(wkcl_app_t app, const char* url);

/*
 * Load a local HTML file into the WebView.
 * path: absolute or relative file path (UTF-8)
 */
void wkcl_load_file(wkcl_app_t app, const char* path);

/* ── JavaScript ──────────────────────────────────────────────── */

/*
 * Evaluate JavaScript in the WebView.
 * js: UTF-8 JavaScript string
 * The result is discarded (fire-and-forget).
 */
void wkcl_eval_js(wkcl_app_t app, const char* js);

/* ── Bridge ──────────────────────────────────────────────────── */

/*
 * Register the bridge callback.
 * All JS invocations route through this single callback, which
 * dispatches based on the command name.
 */
void wkcl_set_bridge_callback(wkcl_app_t app,
                               wkcl_bridge_callback_t callback,
                               void* userdata);

/* ── Window Management ───────────────────────────────────────── */

/*
 * Set the window title.
 */
void wkcl_set_title(wkcl_app_t app, const char* title);

/*
 * Resize the window.
 */
void wkcl_set_size(wkcl_app_t app, int width, int height);

/*
 * Set whether the window is resizable.
 */
void wkcl_set_resizable(wkcl_app_t app, int resizable);

#ifdef __cplusplus
}
#endif

#endif /* WEBKIT_CL_H */
