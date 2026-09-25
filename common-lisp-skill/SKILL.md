---
name: common-lisp
description: Write Common Lisp in Mark Watson's style for the loving-common-lisp project. Quicklisp, ASDF, UIOP / Dexador / QURI / cl-json, file IO, and a deliberately plain no-LOOP style.
---

# Skill: Common Lisp in Mark Watson's Style

Writing Common Lisp for Mark Watson's `loving-common-lisp` project. The audience
is a model strong in Python and TypeScript and weak in Lisp, so treat the rules
below as absolute.

## 0. How you work here — you do not run code

**The developer runs everything. You never execute Lisp, and you never claim you did.**

1. **Forms to evaluate** — the smallest set that proves the point, in one `lisp`
   block, in evaluation order, starting with any `ql:quickload`. Say what you
   expect back.
2. **A file to load** — write it, then ask the developer to run
   `sbcl --script path/to/file.lisp` and paste what it prints.

Ask for the specific thing you need ("paste the value of the last form"), not "the
output". Verify the risky parts only; read the real error before guessing. Until
output comes back, say the code is unverified. `sbcl --script` skips `~/.sbclrc`,
so it works whatever the developer's Quicklisp setup looks like; if the file uses
`uiop:` or `asdf:`, put `(require :asdf)` on its first line.

---

## 1. The ten rules

1. **Never write `loop`** (see 2.5). Leave old `loop` forms in `src/` alone unless asked.
2. **Every `(` must be closed** — an unbalanced paren is the most common failure.
3. **Locals must be declared** by `let`/`let*`/`dolist`/`dotimes`/`defun` args; there is no "assign and it exists".
4. **Use `format`, not `print`**, for anything a human reads.
5. **A function returns its last form**; use `return`/`return-from` to exit early.
6. **Only `nil` is false.** `0`, `""`, and `#()` are all true; there is no separate `False`.
7. **Strings compare with `string=` or `equal`**; numbers with `=`.
8. **Hash tables with string keys need `:test #'equal`.**
9. **`if` takes both branches**; with one branch use `when`/`unless`.
10. **Match the surrounding file** — its package, indentation, and naming.

---

## 2. Mark's style

### 2.1 Layout and naming

```lisp
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License
;;; One-line description of this file.

(in-package #:my-package)

(defun my-function (argument)
  "Docstring in the imperative mood, one sentence."
  (format nil "value is ~a" (* argument 2)))
```

Two spaces per indent; closing parens stack on the last line. `;;;` headers, `;;`
inside functions, `;` at end of line. A docstring is the **first** form of
`defun`/`defclass`/`defstruct`/`defvar`/`defparameter`. Names: `fetch-url`;
predicates `valid-uri-p`; globals `*earmuffs*`; constants `+plus-signs+`;
accessors `type-slot`.

### 2.2 Packages and globals

Use `#:name` (uninterned symbol) for the package name, and **export everything the
outside world calls** — otherwise callers get `The symbol X is not external`.

```lisp
(defpackage #:my-system
  (:use #:cl)
  (:export #:run #:response #:response-status))

(in-package #:my-system)

(defparameter *base-url* "https://api.example.com"  ; resets on reload: config
  "Base URL for API requests.")
(defvar *cache* (make-hash-table :test #'equal)     ; set when unbound: state
  "Memoisation cache. Keeps its value when the file is reloaded.")
```

Inside the package call your own functions bare; outside use `my-system:run`.
`cl-user` is the wrong place for real code.

### 2.3 Functions

Use **either** `&optional` **or** `&key`, never both.

```lisp
(defun greet (name &key (greeting "Hello") (excited-p nil))
  "Return a greeting string for NAME."
  (format nil "~a, ~a~a" greeting name (if excited-p "!" ".")))
```

```lisp
(greet "Mark")                    ; => "Hello, Mark."
(greet "Mark" :excited-p t)       ; => "Hello, Mark!"
(greet "Mark" :nope 1)            ; ERROR: Unknown &KEY argument: :NOPE
```

> **Never write `(defun f (x &optional y &key z) ...)`.** The optional parameter
> swallows the first keyword, so `(f 1 :z 2)` binds `:Z` to `y` and then signals
> `odd number of &KEY arguments`.

Order is `&optional`, then `&rest`, then `&key`. Never name a function after a CL
symbol — `report`, `count`, `remove`, `find`, `sort`, `length` are locked, and
redefining one signals `Lock on package COMMON-LISP violated`. Add a suffix.

### 2.4 Conditionals

```lisp
(if test then else)                   ; exactly 2 or 3 parts
(when test body...)                   ; no else branch
(unless test body...)                 ; negated when
(cond (test1 body1...) (test2 body2...) (t default...))
(case value (1 :one) (2 :two) (t :other))
```

### 2.5 No `loop` — the translation table

Mark, in `manuscript/loop_macros.md`: *"a complex looping macro that I almost
never use in my own code because it does not look Lisp like."*

| Do not write | Write instead |
| --- | --- |
| `(loop for x in xs do (f x))` | `(dolist (x xs) (f x))` |
| `(loop for i from 0 below n do ...)` / `(loop repeat n do ...)` | `(dotimes (i n) ...)` |
| `(loop while test do ...)` | `(do () ((not test)) ...)` |
| `(loop for x in xs collect (f x))` | `(mapcar #'f xs)` |
| `(loop for x in xs when (p x) collect x)` | `(remove-if-not #'p xs)` |
| `(loop for x in xs unless (p x) collect x)` | `(remove-if #'p xs)` |
| `(loop for x in xs sum x)` | `(reduce #'+ xs)` or an accumulator |
| `(loop for x in xs count (p x))` | `(count-if #'p xs)` |
| `(loop for x in xs always (p x))` | `(every #'p xs)` |
| `(loop for k being the hash-keys of h ...)` | `maphash` + collect (see 2.6) |

The accumulator idiom you will use constantly — `push` builds the list backwards,
`nreverse` at the end, and the third argument of `dolist`/`dotimes` is the return
value:

```lisp
(defun squares-of-evens (numbers)
  "Return the squares of the even members of NUMBERS."
  (let ((result nil))
    (dolist (n numbers (nreverse result))
      (when (evenp n) (push (* n n) result)))))
```

### 2.6 Iteration, hash tables, structs

```lisp
(dolist (item items) (format t "~a~%" item))
(let ((sum 0)) (dolist (n '(1 2 3) sum) (incf sum n)))   ; returns SUM
(dotimes (i 5) (format t "~d~%" i))                      ; 0 .. 4
(dolist (x '(1 2 3 4)) (when (= x 3) (return)))          ; early exit
;; do: (do ((var init step)...) ((test result...) body...)
(do ((i 0 (1+ i)) (acc nil (cons i acc)))
    ((= i 3) (nreverse acc)))                            ; => (0 1 2)
```

```lisp
(defun hash-table-keys (table)
  "Return a list of the keys in TABLE."
  (let ((keys nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) table)
    (nreverse keys)))

(defstruct point "A 2D point." x y)
;; => (make-point :x 1 :y 2)  (point-x p)  (setf (point-x p) 7)  (point-p p)
```

The `(declare (ignore v))` is needed or SBCL warns about the unused variable.
`defstruct` defines `make-<name>`, `<name>-<slot>`, and `<name>-p`; use it instead
of a class when there is no behaviour.

---

## 3. Language core

### 3.1 Syntax

```lisp
(fn arg1 arg2)          ; call FN with two arguments
; comment to end of line          #| block comment |#
"a string"              #\a          ; the character a
:keyword                ; self-evaluating keyword symbol
'symbol                 ; the symbol, not its value
'(1 2 3)                ; a literal list      #'function-name  ; function object
#(1 2 3)                ; a vector            #p"/a/path"      ; a pathname
42   3.14   1.0d0   1/3 ; integer, single-float, double-float, ratio
nil   t                 ; false / true (also '() for nil)
```

The reader upcases symbols, so `Foo` and `FOO` are the same symbol. `1.0` is a
*single*-float; write `1.0d0` for a double.

### 3.2 Truth and equality — read this twice

Only `nil` is false. `0`, `""`, `#()`, and `:false` are all **true**.

| Test | Use it for | Example |
| --- | --- | --- |
| `=` | numbers only | `(= 1 1.0)` → `T` |
| `char=` / `char-equal` | characters, case-sensitive / not | `(char-equal #\a #\A)` → `T` |
| `string=` / `string-equal` | strings | `(string= "a" "a")` → `T` |
| `eq` | identical object, same symbol | `(eq 'a 'a)` → `T` |
| `eql` | numbers and characters of the same type | `(eql 1 1.0)` → `NIL` |
| `equal` | lists, strings, structures — **the default choice** | `(equal '(1 2) '(1 2))` → `T` |
| `equalp` | like `equal`, case/type-insensitive, compares vectors | `(equalp #(1 2) #(1 2))` → `T` |
| `null` | is it nil | `(null nil)` → `T` |
| `member` | list membership | `(member 2 '(1 2 3))` → `(2 3)` |

Two traps: `(equal #(1 2) #(1 2))` is `NIL` — `equal` does not descend into
vectors, so use `equalp`. And boolean functions return a generalised truth value,
not necessarily `t`: `(member 3 '(1 2 3))` → `(3)`, `(search "zz" "abc")` → `NIL`.

### 3.3 Locals, recursion, multiple values

```lisp
(let ((a 1) (b 2)) (+ a b))       ; bindings computed in parallel
(let* ((a 1) (b (+ a 1))) b)      ; later bindings see earlier ones
(let ((total 0)) (incf total 5) (decf total 2) (setf total 0))   ; => 5, 3, 0

(defun count-items (list)
  "Count LIST without a loop."
  (labels ((walk (rest count)
             (if (null rest) count (walk (cdr rest) (1+ count)))))
    (walk list 0)))

(multiple-value-bind (q r) (floor 7 2)          ; => 3 rem 1
  (format nil "~d rem ~d" q r))
```

`let` for independent bindings, `let*` when one depends on another. `setf` assigns
to anything: variables, `(aref v i)`, `(gethash k h)`, `(point-x p)`. `labels`
defines local recursive functions, `flet` non-recursive ones, `(lambda (x) ...)`
anonymous ones. `gethash`, `read-from-string`, `dex:get`, and `uiop:run-program`
return extra values — capture them with `multiple-value-bind`, not `let`.

### 3.4 Lists, vectors, sequences

```lisp
(cons 1 '(2 3))          ; => (1 2 3)      (list 1 2 3)  ; => (1 2 3)
(car '(1 2 3))           ; => 1  (also FIRST)      (cdr xs)  ; => (2 3)  (also REST)
(length '(1 2 3))        ; => 3            (nth 1 '(1 2 3))  ; => 2
(append '(1 2) '(3 4))   ; => (1 2 3 4)    (reverse '(1 2 3))  ; => (3 2 1)
(subseq '(1 2 3 4) 1 3)  ; => (2 3)        (last '(1 2 3))  ; => (3)
(push 1 xs)              ; (setf xs (cons 1 xs))    (pop xs)  ; returns (car xs)
(sort (copy-list xs) #'<)                  ; destructive: copy first
(assoc "b" alist :test #'string=)          ; => ("b" . 2)
(aref v 1)                                 ; read element 1   (setf (aref v 1) 9)
(elt v 0)                                  ; works on vectors and lists
```

`mapcar`, `remove-if`, `remove-if-not`, `reduce`, `find-if`, `position`,
`count-if`, `some`, and `every` are the sequence functions you reach for instead
of `loop` — see the table in 2.5. `sort` and `nreverse` destroy their argument;
copy first when the list is shared: `(sort (copy-list xs) #'<)`.

```lisp
(let ((table (make-hash-table :test #'equal)))   ; EQUAL for string keys
  (setf (gethash "a" table) 1)                   ; write
  (gethash "a" table)                            ; => 1, T
  (gethash "missing" table :none)                ; => :NONE
  (remhash "a" table))
```

The default hash-table `:test` is `#'eql`, which will **not** find string keys
built at runtime — pass `:test #'equal` for strings, `#'eq` for keywords.

### 3.5 CLOS, and alists

```lisp
(defclass animal ()
  ((name :initarg :name :accessor animal-name)
   (sound :initarg :sound :initform "..." :accessor animal-sound)))
(defmethod speak ((a animal))
  (format nil "~a says ~a" (animal-name a) (animal-sound a)))
(defclass dog (animal) ())
(defmethod speak ((d dog)) (format nil "~a barks" (animal-name d)))
;; (speak (make-instance 'dog :name "Rex"))  => "Rex barks"
```

Slots need `:initarg` to be settable from `make-instance` and `:accessor` (or
`:reader`) to be readable; `:initform` gives a default. `defmethod` dispatches on
the class of its arguments, and a subclass method overrides the parent.

```lisp
;; alist: a list of (key . value) conses — the shape decoded JSON takes
(defparameter *config* '(("host" . "localhost") ("port" . 8080)))
(cdr (assoc "host" *config* :test #'string=))     ; => "localhost"
(getf '(:name "Mark" :age 60) :name)              ; plist => "Mark"
```

---

## 4. Strings and FORMAT

```lisp
(length "hello")                       ; => 5
(subseq "hello" 1 3)                   ; => "el"
(concatenate 'string "foo" "-" "bar")  ; => "foo-bar"
(string-downcase "ABC")                ; => "abc"  (also string-upcase)
(string-trim '(#\Space #\Tab #\Newline) "  hi  ")   ; => "hi"
(search "ll" "hello")                  ; => 2, or NIL
(position #\l "hello")                 ; => 2
(char "abc" 1)                         ; => #\b
(parse-integer "42")                   ; => 42   :radix 16 parses "ff"
```

Never modify a string literal; build a new string. There is no built-in `split`:

```lisp
(defun split-string (string delimiter)
  "Split STRING on the character DELIMITER. Returns a list of strings."
  (let ((parts nil)
        (start 0))
    (dotimes (i (length string))
      (when (char= (char string i) delimiter)
        (push (subseq string start i) parts)
        (setf start (1+ i))))
    (push (subseq string start) parts)
    (nreverse parts)))

(split-string "a,b,c" #\,)     ; => ("a" "b" "c")
```

Joining is a `format` one-liner: `(format nil "~{~a~^, ~}" '("a" "b"))` → `"a, b"`.
`(format destination control-string args...)`: destination `t` prints to stdout
and returns `nil`, destination `nil` returns the string and prints nothing.

| Directive | Meaning | Directive | Meaning |
| --- | --- | --- | --- |
| `~a` | aesthetic (human) form | `~d` | decimal integer |
| `~s` | `read`-able form, strings quoted | `~x` / `~o` / `~b` | hex / octal / binary |
| `~f`, `~,2f`, `~$` | float, 2 decimals, money | `~5,'0d` | pad to width 5 with `0` |
| `~10a` | value padded to width 10 | `~%` | newline |
| `~&` | fresh line if not at line start | `~:p` | pluralise previous integer |
| `~{~a~^, ~}` | iterate a list; `~^` skips the last separator | `~~` | a literal tilde |

```lisp
(format nil "~a=~d" "x" 42)              ; => "x=42"
(format nil "~,2f" 3.14159)              ; => "3.14"
(format nil "~{~a~^, ~}" '(1 2 3))       ; => "1, 2, 3"
(format nil "~5,'0d" 42)                 ; => "00042"
(format nil "~s" '(1 "a"))               ; => "(1 \"a\")"
(with-output-to-string (out)             ; build a string
  (dolist (line lines) (format out "~a~%" line)))
```

---

## 5. File I/O

Mark prefers plain **strings** for filenames, not pathname objects. Relative
filenames resolve against the working directory, which is *not* the script's
directory — use an absolute path or
`(uiop:merge-pathnames* "data/x.txt" *load-truename*)`.

```lisp
;; Read. Needs (require :asdf) at the top of the file.
(uiop:read-file-string #p"data/input.txt")     ; => "the whole file"
(uiop:read-file-lines #p"data/input.txt")      ; => ("line 1" "line 2")

;; Write. NOTE: uiop has no write-file-string.
(with-open-file (out #p"data/output.txt" :direction :output
                     :if-exists :supersede :if-does-not-exist :create
                     :external-format :utf-8)
  (format out "line ~d~%" 1))
```

`:if-exists` is `:supersede` (overwrite), `:append`, or `:error`; omit it when the
file exists and you get an error. **Always use `with-open-file` rather than
`open`** — it closes the stream even if the body signals.

`(read-line stream nil nil)` returns `nil` at end of file (the two `nil`s mean "no
error" and "return nil at EOF"); `(read stream nil nil)` does the same for forms.
Reading lines without `loop`:

```lisp
(with-open-file (in path :direction :input :if-does-not-exist nil)
  (when in
    (let ((lines nil)
          (line nil))
      (do () ((null (setf line (read-line in nil nil))) (nreverse lines))
        (when (plusp (length line)) (push line lines))))))
```

---

## 6. Conditions and shell commands

```lisp
(handler-case
    (dex:get "https://example.com")
  (dex:http-request-not-found (e)          ; a subclass of the next clause
    (format t "~&404: ~a~%" (dex:response-body e)))
  (dex:http-request-failed (e)             ; any non-2xx response
    (format t "~&HTTP ~a: ~a~%" (dex:response-status e) (dex:response-body e)))
  (error (e) (format t "~&request failed: ~a~%" e)))
```

`(error (e) ...)` catches everything; `ignore-errors` returns `nil` plus the
condition. `unwind-protect` runs cleanup on success and failure.
`(check-type count integer)` signals a correctable error, `(assert (> count 0))`
signals when false. Define your own with `(define-condition my-error (error)
((msg :initarg :msg :reader my-error-msg)))`, then `(error 'my-error :msg "t")`.

```lisp
(uiop:run-program '("ls" "-l") :output :string)          ; stdout as a string
;; with :output :string :error-output :string :ignore-error-status t you get
;; stdout, stderr and the exit code as three values — bind them with
;; multiple-value-bind.
```

Two traps: `:input` with a string is a **filename**, not data — use
`:input (make-string-input-stream "text")`. Without `:ignore-error-status t` a
non-zero exit signals `uiop:subprocess-error`.

---

## 7. Packages, systems, libraries

### 7.1 A script, and an ASDF system

```lisp
;;; scratch.lisp -- run with: sbcl --script scratch.lisp
(require :asdf)                 ; needed before any uiop: reference
(format t "~&~a~%" (uiop:read-file-string #p"/etc/hosts"))
```

A system is `.asd` + `package.lisp` + `my-system.lisp` + `tests.lisp` + `run.lisp`.
Inside a `.asd` there is no `in-package`:

```lisp
(asdf:defsystem #:my-system
  :description "Short description of what this does."
  :author "Mark Watson" :license "Apache 2" :version "1.0.0" :serial t
  :depends-on (#:uiop)              ; add #:dexador #:quri only when used
  :components ((:file "package") (:file "my-system"))
  :in-order-to ((test-op (test-op "my-system/tests"))))

(asdf:defsystem #:my-system/tests
  :depends-on (#:my-system) :serial t
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :my-system/tests :run-tests)))
```

`tests.lisp` starts with `(defpackage #:my-system/tests (:use #:cl #:my-system)
(:export #:run-tests))` and `(in-package #:my-system/tests)`, then asserts — no
test framework is needed:

```lisp
(defun run-tests ()
  "Run every check. Signals on failure, returns T on success."
  (assert (string= (greet "world") "Hello, world!"))
  (format t "~&my-system: all tests passed~%")
  t)
```

To exercise a system, give the developer a `run.lisp` with `(require :asdf)`,
`(asdf:load-asd (truename #p"my-system.asd"))`, `(asdf:load-system "my-system")`,
`(asdf:test-system "my-system")`, and ask for `sbcl --script run.lisp`. Every name
in `:depends-on` must be findable or loading fails with `Component #:DEXADOR not
found` — keep it to `#:uiop` until the task needs a library.

### 7.2 Quicklisp

```lisp
(ql:quickload :dexador)                     ; load one system
(ql:quickload '(:dexador :quri) :silent t)  ; load several, quietly
(ql:system-apropos "json")                  ; search the dist
```

Put `ql:quickload` in the forms you hand over or at the top of the script.
`:silent t` suppresses chatter; a keyword (`:dexador`) or string (`"dexador"`)
both work. The first load downloads and compiles, so it is slow. Mark's book
systems live in `~/quicklisp/local-projects/`:

```lisp
(pushnew #p"/Users/markw/GITHUB/loving-common-lisp/src/" ql:*local-project-directories*
         :test #'equal)
(ql:quickload :myutils :silent t)
```

### 7.3 uiop, dexador, quri

**uiop** ships with ASDF, so `(require :asdf)` first. See 5 for file functions;
also `(uiop:getcwd)`, `(uiop:getenv "HOME")`,
`(uiop:native-namestring #p"/tmp/x")`, `(uiop:quit 1)`.

**dexador** — `(ql:quickload :dexador)`. `dex:get` returns body, then status,
headers, uri; `:put`, `:patch`, `:delete`, `:fetch` (to a file) mirror it. Response
header names are lowercase strings: `(gethash "content-type" headers)`. Condition
accessors: `dex:response-status`, `dex:response-body`, `dex:response-headers`.
**`dex:get` has no `:params` keyword** — build the URL with `quri`.

```lisp
(dex:get "https://example.com/api"
         :headers '(("Accept" . "application/json")
                    ("Authorization" . "Bearer TOKEN"))
         :connect-timeout 10 :read-timeout 60)

(dex:post "https://example.com/form"          ; alist => form-encoded
          :content '(("a" . "1") ("b" . "two")))

(dex:post "https://example.com/json"          ; JSON
          :content (cl-json:encode-json-to-string '(("name" . "Mark")))
          :headers '(("Content-Type" . "application/json")))
```

**quri** — URIs and percent-encoding, `(ql:quickload :quri)`.

```lisp
;; Parse with (quri:uri "https://x/y?z=1"), then read it back with these;
;; (quri:uri-query-params u) => (("z" . "1")), an already-decoded alist.
(quri:uri-scheme u)  (quri:uri-host u)  (quri:uri-port u)
(quri:uri-path u)    (quri:uri-query u) (quri:render-uri u)

;; Build. The keyword is :query (a string), NOT :query-params.
(quri:render-uri
 (quri:make-uri :scheme "https" :host "example.com" :path "/search"
                :query (quri:url-encode-params '(("q" . "common lisp")))))
;; => "https://example.com/search?q=common%20lisp"

(quri:url-encode "common lisp")                   ; => "common%20lisp"
(quri:url-encode "common lisp" :space-to-plus t)  ; => "common+lisp"
(quri:url-decode "common%20lisp")                 ; => "common lisp"
```

quri + dexador + cl-json is how you call a JSON API: build the URL, `dex:get` it,
and decode the body with `cl-json:decode-json-from-string`.

### 7.4 cl-json and alexandria

```lisp
;; (ql:quickload :cl-json)
(cl-json:encode-json-to-string '(("name" . "Mark") ("n" . 3)))
;; => "{\"name\":\"Mark\",\"n\":3}"
(cl-json:decode-json-from-string "{\"a\": 1, \"b\": \"two\"}")
;; => ((:A . 1) (:B . "two"))    keyword keys, alist shape; lists => arrays
```

Encoding: alists → objects, other lists → arrays, `t` → `true`, `nil` → `null`.
Decoding: objects → alists with **keyword** keys, arrays → lists, and JSON
`false`/`null` both become `nil`. `(cl-json:encode-json-to-string :false)` gives
the *string* `"false"`, not `false`.

**alexandria** — `(ql:quickload :alexandria)`: `(alexandria:hash-table-keys table)`,
`(alexandria:read-file-into-string #p"x.txt")`,
`(alexandria:write-string-into-file "text" #p"x.txt" :if-exists :supersede)`,
`(alexandria:flatten '(1 (2 (3 4)) 5))` → `(1 2 3 4 5)`.

**Mark's own libraries** (`entities`, `gemini`, `kbnlp`, `lightpanda`, `ollama`,
`sparql`, `litelm`, ...) are documented in `src/cl-book-apis/SKILL.md` — read it
before reimplementing anything.

---

## 8. Python / TypeScript to Common Lisp

| Python / TypeScript | Common Lisp |
| --- | --- |
| `def f(a, b=1)` / `def f(*args, **kw)` | `(defun f (a &optional (b 1)) ...)` / `(&rest args &key ...)` |
| `[f(x) for x in xs]` / `[x for x in xs if p(x)]` | `(mapcar #'f xs)` / `(remove-if-not #'p xs)` |
| `xs + ys` / `sum(xs)` / `sorted(xs)` | `(append xs ys)` / `(reduce #'+ xs)` / `(sort (copy-list xs) #'<)` |
| `f"{a}: {b}"` / `print(x)` | `(format nil "~a: ~a" a b)` / `(format t "~a~%" x)` |
| `str(x)` / `int(s)` / `float(s)` | `(princ-to-string x)` / `(parse-integer s)` / `(coerce (read-from-string s) 'double-float)` |
| `s.upper()` / `s.strip()` / `s.split(",")` | `(string-upcase s)` / `(string-trim '(#\Space) s)` / `(split-string s #\,)` |
| `",".join(xs)` / `None` / `True` / `False` | `(format nil "~{~a~^,~}" xs)` / `nil` / `t` / `nil` |
| `d = {}` / `d[k] = v` / `d[k]` / `d.get(k, d)` | `(make-hash-table :test #'equal)` / `(setf (gethash k d) v)` / `(gethash k d)` / `(gethash k d d)` |
| `k in d` / `del d[k]` / `d.keys()` | `(nth-value 1 (gethash k d))` / `(remhash k d)` / `(hash-table-keys d)` |
| `try/except` / `raise ValueError("x")` | `(handler-case ... (E (e) ...))` / `(error "x")` |
| `json.dumps` / `json.loads` / `requests.get(url)` | `(cl-json:encode-json-to-string o)` / `(cl-json:decode-json-from-string s)` / `(dex:get url)` |
| `os.environ["K"]` / `open(p).read()` | `(uiop:getenv "K")` / `(uiop:read-file-string p)` |
| `//` / `%` / `**` / `x += 1` | `(floor a b)` / `(mod a b)` / `(expt a b)` / `(incf x)` |

---

## 9. Before you answer — the checklist

1. **Parens balance.** Every top-level form's parens stack at the end of its last line.
2. **No `loop` in new code.** Re-read what you wrote.
3. **Every package prefix is loaded** — `dex:`, `quri:` need a `ql:quickload`.
4. **Every local is bound** by `let`/`let*`/`defun` args/`dolist`/`dotimes`/`do`.
5. **`string=`/`equal` for strings**, `=` for numbers, `:test #'equal` for string keys.
6. **`if` has both branches**, or you used `when`/`unless`.
7. **You checked the failure path** — a missing file, an empty list, a 404.
8. **Docstrings** on public functions, `*earmuffs*` on globals.
9. **You did not invent library functions** — check `(fboundp 'pkg:name)` or list
   exports with `do-external-symbols`.
10. **You told the developer what to run** and did not claim to have run it.
