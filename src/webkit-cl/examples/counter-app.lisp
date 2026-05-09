;;; counter-app.lisp — Interactive bridge demo for webkit-cl
;;;
;;; A counter app that demonstrates the JS ↔ Lisp bridge:
;;;   - "increment" / "decrement" commands modify Lisp-side state
;;;   - "get-count" retrieves the current value
;;;   - Results flow back to JavaScript and update the UI
;;;
;;; This mirrors zero-native's bridge pattern where native handlers
;;; process commands and return JSON responses.

(require :asdf)
(push (make-pathname :directory (pathname-directory
                                  (make-pathname
                                    :directory (butlast (pathname-directory *load-pathname*)))))
      asdf:*central-registry*)
(asdf:load-system :webkit-cl)

;;; ── Application State ──────────────────────────────────────────

(defvar *counter* 0)

;;; ── Bridge Handlers ────────────────────────────────────────────

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

(webkit-cl:register-handler "get-count"
  (lambda (payload)
    (declare (ignore payload))
    (format nil "{\"count\": ~d}" *counter*)))

(webkit-cl:register-handler "get-system-info"
  (lambda (payload)
    (declare (ignore payload))
    (format nil "{\"lisp\": \"~a\", \"version\": \"~a\", \"machine\": \"~a\"}"
            (lisp-implementation-type)
            (lisp-implementation-version)
            (machine-type))))

;;; ── Run the App ────────────────────────────────────────────────

(webkit-cl:with-app (:title "Counter — webkit-cl" :width 500 :height 520)
  (webkit-cl:load-html
   "<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, system-ui, sans-serif;
    background: #0a0a0a;
    color: #fafafa;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 100vh;
    gap: 32px;
    user-select: none;
    -webkit-user-select: none;
  }
  .counter-display {
    font-size: 6rem;
    font-weight: 800;
    font-variant-numeric: tabular-nums;
    letter-spacing: -4px;
    background: linear-gradient(180deg, #fff 0%, rgba(255,255,255,0.5) 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    transition: transform 0.15s ease;
    min-width: 200px;
    text-align: center;
  }
  .counter-display.bump {
    transform: scale(1.1);
  }
  .controls {
    display: flex;
    gap: 12px;
  }
  button {
    font-size: 1.1rem;
    font-weight: 600;
    padding: 12px 28px;
    border: none;
    border-radius: 12px;
    cursor: pointer;
    transition: all 0.2s ease;
    font-family: inherit;
  }
  button:active {
    transform: scale(0.95);
  }
  .btn-primary {
    background: linear-gradient(135deg, #7c3aed, #a855f7);
    color: white;
    box-shadow: 0 4px 14px rgba(124,58,237,0.4);
  }
  .btn-primary:hover {
    box-shadow: 0 6px 20px rgba(124,58,237,0.6);
    transform: translateY(-1px);
  }
  .btn-primary:active {
    transform: scale(0.95) translateY(0);
  }
  .btn-danger {
    background: linear-gradient(135deg, #dc2626, #ef4444);
    color: white;
    box-shadow: 0 4px 14px rgba(220,38,38,0.3);
  }
  .btn-danger:hover {
    box-shadow: 0 6px 20px rgba(220,38,38,0.5);
    transform: translateY(-1px);
  }
  .btn-secondary {
    background: rgba(255,255,255,0.08);
    color: rgba(255,255,255,0.7);
    border: 1px solid rgba(255,255,255,0.1);
  }
  .btn-secondary:hover {
    background: rgba(255,255,255,0.12);
    color: white;
  }
  .info {
    font-size: 0.8rem;
    color: rgba(255,255,255,0.3);
    text-align: center;
    line-height: 1.6;
  }
  .info span {
    color: rgba(255,255,255,0.5);
  }
  h2 {
    font-size: 0.9rem;
    font-weight: 500;
    color: rgba(255,255,255,0.4);
    letter-spacing: 3px;
    text-transform: uppercase;
  }
</style>
</head>
<body>
  <h2>Counter</h2>
  <div class='counter-display' id='counter'>0</div>
  <div class='controls'>
    <button class='btn-danger' onclick='decrement()'>− Minus</button>
    <button class='btn-secondary' onclick='reset()'>Reset</button>
    <button class='btn-primary' onclick='increment()'>+ Plus</button>
  </div>
  <div class='info' id='info'>Loading system info...</div>

  <script>
    const display = document.getElementById('counter');
    const info = document.getElementById('info');

    function updateDisplay(count) {
      display.textContent = count;
      display.classList.add('bump');
      setTimeout(() => display.classList.remove('bump'), 150);
    }

    async function increment() {
      const result = await window.webkit_cl.invoke('increment', {});
      updateDisplay(result.count);
    }

    async function decrement() {
      const result = await window.webkit_cl.invoke('decrement', {});
      updateDisplay(result.count);
    }

    async function reset() {
      const result = await window.webkit_cl.invoke('reset', {});
      updateDisplay(result.count);
    }

    // Load system info on startup
    setTimeout(async () => {
      try {
        const sysInfo = await window.webkit_cl.invoke('get-system-info', {});
        info.innerHTML =
          'Powered by <span>' + sysInfo.lisp + ' ' + sysInfo.version + '</span>' +
          '<br>Architecture: <span>' + sysInfo.machine + '</span>';
      } catch(e) {
        info.textContent = 'JS ↔ Lisp bridge active';
      }
    }, 500);
  </script>
</body>
</html>"))
