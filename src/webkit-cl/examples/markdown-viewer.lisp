;;; markdown-viewer.lisp — Local file viewer example for webkit-cl
;;;
;;; Demonstrates loading a local HTML file and using the bridge
;;; to list and read files from the filesystem via Lisp.

(require :asdf)
(push (make-pathname :directory (pathname-directory
                                  (make-pathname
                                    :directory (butlast (pathname-directory *load-pathname*)))))
      asdf:*central-registry*)
(asdf:load-system :webkit-cl)

;;; ── Bridge Handlers ────────────────────────────────────────────

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

;;; ── Run the App ────────────────────────────────────────────────

(webkit-cl:with-app (:title "Markdown Viewer — webkit-cl" :width 900 :height 700)
  (webkit-cl:load-html
   "<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, system-ui, sans-serif;
    background: #111;
    color: #e5e5e5;
    display: flex;
    height: 100vh;
  }
  .sidebar {
    width: 260px;
    min-width: 260px;
    background: #1a1a1a;
    border-right: 1px solid rgba(255,255,255,0.06);
    display: flex;
    flex-direction: column;
    overflow-y: auto;
  }
  .sidebar-header {
    padding: 20px 16px 12px;
    font-size: 0.75rem;
    font-weight: 600;
    letter-spacing: 2px;
    text-transform: uppercase;
    color: rgba(255,255,255,0.3);
  }
  .file-item {
    padding: 10px 16px;
    font-size: 0.9rem;
    cursor: pointer;
    color: rgba(255,255,255,0.6);
    transition: all 0.15s;
    border-left: 3px solid transparent;
  }
  .file-item:hover {
    background: rgba(255,255,255,0.04);
    color: white;
  }
  .file-item.active {
    background: rgba(124,58,237,0.1);
    color: #a78bfa;
    border-left-color: #7c3aed;
  }
  .file-item .name {
    font-weight: 500;
  }
  .file-item .path {
    font-size: 0.75rem;
    color: rgba(255,255,255,0.25);
    margin-top: 2px;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .content {
    flex: 1;
    padding: 32px 48px;
    overflow-y: auto;
    font-size: 0.95rem;
    line-height: 1.7;
  }
  .content pre {
    background: rgba(255,255,255,0.04);
    border: 1px solid rgba(255,255,255,0.08);
    border-radius: 8px;
    padding: 16px 20px;
    overflow-x: auto;
    font-family: 'SF Mono', 'Fira Code', monospace;
    font-size: 0.85rem;
    white-space: pre-wrap;
    word-wrap: break-word;
    color: #c4b5fd;
  }
  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 100%;
    color: rgba(255,255,255,0.2);
    font-size: 1.1rem;
    gap: 8px;
  }
  .empty-state .icon {
    font-size: 3rem;
    margin-bottom: 8px;
  }
</style>
</head>
<body>
  <div class='sidebar'>
    <div class='sidebar-header'>Files</div>
    <div id='file-list'>
      <div class='empty-state' style='height:200px;font-size:0.85rem;'>
        Loading...
      </div>
    </div>
  </div>
  <div class='content' id='content'>
    <div class='empty-state'>
      <div class='icon'>📄</div>
      <div>Select a file to view</div>
      <div style='font-size:0.85rem;color:rgba(255,255,255,0.15)'>
        Markdown files from the project directory
      </div>
    </div>
  </div>

  <script>
    const fileList = document.getElementById('file-list');
    const content = document.getElementById('content');

    function escapeHtml(str) {
      return str.replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
    }

    function basename(path) {
      return path.split('/').pop();
    }

    async function loadFileList() {
      try {
        const result = await window.webkit_cl.invoke('list-files', { directory: '.' });
        if (result.files && result.files.length > 0) {
          fileList.innerHTML = result.files.map(f =>
            '<div class=\"file-item\" onclick=\"loadFile(\\'' + f.replace(/'/g, '\\\\\\'') + '\\')\">' +
            '<div class=\"name\">' + escapeHtml(basename(f)) + '</div>' +
            '<div class=\"path\">' + escapeHtml(f) + '</div>' +
            '</div>'
          ).join('');
        } else {
          fileList.innerHTML =
            '<div class=\"empty-state\" style=\"height:200px;font-size:0.85rem;\">' +
            'No .md files found</div>';
        }
      } catch(e) {
        fileList.innerHTML =
          '<div class=\"empty-state\" style=\"height:200px;font-size:0.85rem;\">' +
          'Error: ' + e + '</div>';
      }
    }

    async function loadFile(path) {
      // Highlight active item
      document.querySelectorAll('.file-item').forEach(el => {
        el.classList.remove('active');
        if (el.querySelector('.path').textContent === path) {
          el.classList.add('active');
        }
      });

      try {
        const result = await window.webkit_cl.invoke('read-file', { path: path });
        if (result.error) {
          content.innerHTML = '<div class=\"empty-state\">' + escapeHtml(result.error) + '</div>';
        } else {
          content.innerHTML = '<pre>' + escapeHtml(result.content) + '</pre>';
        }
      } catch(e) {
        content.innerHTML = '<div class=\"empty-state\">Error loading file</div>';
      }
    }

    // Load file list on startup
    setTimeout(loadFileList, 300);
  </script>
</body>
</html>"))
