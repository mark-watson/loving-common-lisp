# webkit-cl

A lightweight Common Lisp framework for building native macOS desktop apps with WebKit (WKWebView). Inspired by [zero-native](https://github.com/vercel-labs/zero-native) but focused exclusively on Common Lisp + macOS WebKit.

## Overview

webkit-cl lets you build small GUI applications in Common Lisp using HTML/CSS/JavaScript for the UI, rendered in the system WebKit engine (WKWebView on macOS). Your Lisp code controls the native window and communicates with the web UI through a bidirectional bridge.

**What it implements:**
- Native macOS windows with embedded WKWebView
- Load HTML content inline, from files, or from URLs
- JavaScript ↔ Common Lisp bridge (`window.webkit_cl.invoke()`)
- Window lifecycle management (create, resize, title, close)
- Multiple named bridge command handlers
- JSON-based message passing

**What it does NOT implement:**
- Chrome/Chromium/CEF support
- iOS or Android support
- Linux/GTK support (macOS only)

## Architecture

```
┌─────────────────────────────────────┐
│         Common Lisp App             │
│  (SBCL + CFFI + webkit-cl)         │
├─────────────────────────────────────┤
│       webkit-cl Lisp API            │
│  (app, bridge, window management)   │
├─────────────────────────────────────┤
│          CFFI Bindings              │
│  (webkit-cl-ffi.lisp)              │
├─────────────────────────────────────┤
│      C Shim (webkit_cl.m)          │
│  (Objective-C → C API bridge)       │
├─────────────────────────────────────┤
│    macOS Cocoa + WebKit Framework   │
│  (NSApplication, WKWebView, etc.)   │
└─────────────────────────────────────┘
```

## Prerequisites

- macOS (Apple Silicon or Intel)
- SBCL (Steel Bank Common Lisp)
- Quicklisp with `cffi` and `cl-json` installed

## Quick Start

### 1. Build the native library

```bash
make
```

This compiles `webkit_cl.m` → `libwebkit_cl.dylib`.

### 2. Run an example

```bash
# Hello World — inline HTML
sbcl --load examples/hello-world.lisp

# Counter app — bridge demo with JS ↔ Lisp communication
sbcl --load examples/counter-app.lisp

# Markdown viewer — load HTML from file
sbcl --load examples/markdown-viewer.lisp
```

## API Overview

### Creating an App

```lisp
(webkit-cl:with-app (:title "My App" :width 800 :height 600)
  ;; Load inline HTML
  (webkit-cl:load-html "<h1>Hello from Lisp!</h1>")

  ;; Or load a URL
  (webkit-cl:load-url "https://example.com")

  ;; Or load a local file
  (webkit-cl:load-file "ui/index.html"))
```

### Bridge: JS → Lisp

Register handlers that JavaScript can call:

```lisp
(webkit-cl:register-handler "greet"
  (lambda (payload)
    (format nil "{\"message\": \"Hello, ~a!\"}"
            (cdr (assoc :name payload)))))
```

From JavaScript:
```javascript
const result = await window.webkit_cl.invoke("greet", { name: "World" });
console.log(result.message); // "Hello, World!"
```

### Bridge: Lisp → JS

Evaluate JavaScript from Lisp:

```lisp
(webkit-cl:eval-js "document.title = 'Updated from Lisp'")
```

## Project Structure

```
webkit-cl/
├── Makefile                  # Build the C shim
├── README.md
├── webkit_cl.m               # Objective-C shim (Cocoa + WKWebView → C API)
├── webkit_cl.h               # C API header
├── webkit-cl.asd             # ASDF system definition
├── package.lisp              # Package definition
├── webkit-cl-ffi.lisp        # CFFI bindings to the C shim
├── webkit-cl.lisp            # High-level Lisp API
├── bridge.lisp               # JS ↔ Lisp bridge
└── examples/
    ├── hello-world.lisp      # Minimal inline HTML example
    ├── counter-app.lisp      # Interactive counter with bridge
    └── markdown-viewer.lisp  # Local HTML file viewer
```

## License

Apache-2.0 (matching zero-native)
