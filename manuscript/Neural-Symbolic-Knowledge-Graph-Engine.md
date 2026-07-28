# Building a Neural-Symbolic Knowledge Graph Engine in Common Lisp

This chapter builds NSK, a small knowledge graph engine that reasons in two
ways at once. It answers questions by exact logic when the facts are present,
and it asks a local language model when they are not. The engine  loads under a bare LispWorks or SBCL image, and
runs either as an interactive prompt or as a small web service.

By the end you will understand unification, a triplestore backed by an
append-only log, reader macros that add a query syntax to Lisp, and a neural
fallback path that turns a missing fact into a prompt for a language model.

## Two ways to know things

A knowledge graph stores facts as triples. Each triple has a subject, a
predicate, and an object: `(:mark :wrote :nsk)` reads as "Mark wrote NSK".
Enough triples form a graph, where subjects and objects are nodes and
predicates are the labeled edges between them.

Two traditions answer questions about such a graph.

The **symbolic** tradition treats a query as a logic problem. You give a
pattern with holes in it, such as "who wrote NSK?", and the engine searches the
stored triples for every way to fill the holes. The match is exact. If the fact
is not stored, the answer is "no solutions". This is the model behind Prolog and
Datalog. It is precise, fast, and it never invents an answer. Its weakness is
that it knows only what you told it.

The **neural** tradition treats a query as a prediction. A language model has
read a large body of text and can guess a plausible object for a subject and a
predicate it has never seen stored anywhere. Ask it for the capital of Japan
and it answers Tokyo, even though no one wrote that triple into your database.
Its strength is coverage. Its weakness is that it can be wrong, and it cannot
tell you which facts you actually recorded.

A **neural-symbolic** system joins the two. It tries the exact symbolic match
first, because stored facts are authoritative. Only when the symbolic search
returns nothing, and only for predicates you mark as neural, does it fall back
to the model. NSK draws this line with a single character in the query syntax:
a predicate written `~:capital` may consult the model, while a plain `:capital`
never does.

The rest of the chapter builds this engine one layer at a time. We start with
the matching rule that makes symbolic queries work.

## What we will build

NSK is a stack of small, focused files. Each one adds a single capability, and
each depends only on the ones below it in this list”

```text
main.lisp     command-line flags, then serve or start the REPL
server.lisp   optional REST API (Hunchentoot, loaded on demand)
repl.lisp     the interactive nsk> prompt
query.lisp    pattern matching, conjunctions, the neural fallback hook
neural.lisp   the Ollama client and text-to-triples extraction
reader.lisp   ?var, [triple], and ~neural reader macros
store.lisp    the triplestore: two indices and an append-only log
unify.lisp    logic variables, neural predicates, unification
json.lisp     a dependency-free JSON reader and writer
packages.lisp the package and its exported names
```

A query flows down through these layers and back up:

```text
you type:   [?who :wrote :nsk]
  reader  -> (match-triple (logic-var 'who) :wrote :nsk)
  eval    -> run-query
              -> prove           (thread bindings across clauses)
                  -> match-triple-pattern
                      -> candidate-triples   (narrow by index)
                      -> unify each candidate
                      -> neural fallback if the predicate is ~ and nothing matched
              -> collect bindings for the result variables
  result  -> printed as  who=:MARK
```

The core carries no external dependencies. The neural layer uses an HTTP client
only when one is present, and the server loads Hunchentoot on demand. So the
graph, the query language, and persistence all run with nothing else installed.

## The package

One package holds the whole engine. It uses only `:cl` and optionally the Hunchentoot library and exports the names
that callers and the REPL need. Here is the complete `src/packages.lisp`.

```lisp
;;;; packages.lisp --- Package definition for the NSK engine.

(in-package :cl-user)

(defpackage :nsk
  (:use :cl)
  (:documentation "NSK: Neural-Symbolic Knowledge Graph engine.")
  (:export
   ;; logic variables and neural predicates
   #:logic-var #:logic-var-p #:logic-var-name
   #:neural-predicate #:neural-predicate-p #:neural-predicate-name
   ;; unification
   #:unify #:resolve #:+fail+
   ;; storage
   #:graph #:graph-p #:make-graph #:*graph* #:*log-path*
   #:open-store #:close-store #:add-triple #:remove-triple
   #:all-triples #:triple-count #:candidate-triples
   ;; query engine
   #:match-triple #:match-triple-pattern #:prove #:run-query
   #:ask #:solutions #:query-result #:query-result-p #:query-result-solutions
   ;; reader syntax
   #:*nsk-readtable* #:install-nsk-syntax #:enable-nsk-syntax
   #:nsk-read-from-string
   ;; json helpers
   #:json-encode #:json-parse #:json-get
   ;; neural layer
   #:*ollama-url* #:*ollama-model* #:query-neural-fallback
   #:sanitize-to-keyword #:text->triples #:ingest-text
   ;; server
   #:start-server #:stop-server
   ;; entry points
   #:repl #:main #:version))
```

The export list doubles as a table of contents. Read it top to bottom and you
see the shape of the whole system: variables and predicates, then unification,
then storage, query, reader syntax, JSON, the neural layer, the server, and the
entry points.

## Unification: matching by structure

Unification is the rule that lets a pattern with holes match a concrete fact.
A **logic variable** is a hole. A **substitution** (NSK calls it an
environment) is a set of bindings from variables to values. To unify two terms
means to find a substitution that makes them identical.

Write `t\theta`$ for the result of replacing every variable in a term `t`$ with
its value under a substitution `\theta`$. Then unifying two terms `t_1`$ and
`t_2`$ means finding a `\theta`$ for which

```$
t_1\theta = t_2\theta .
```

For example, unifying the pattern `(?who :wrote :nsk)` with the stored triple
`(:mark :wrote :nsk)` succeeds with `\theta`$ binding `?who` to `:mark`. The
predicate and object already match, so no extra binding is needed.

The algorithm walks both terms together. When it meets a variable, it binds it,
unless the variable already has a binding, in which case it unifies the old
value against the new term. This is the style Peter Norvig uses in *Paradigms
of Artificial Intelligence Programming*. NSK follows it closely.

Two design choices make the code short. First, logic variables are interned by
name, so two reads of `?who` return the same object and simple equality works.
Second, `nil` serves as the empty, successful environment, so a distinct
sentinel `+fail+` marks failure. Here is the complete `src/unify.lisp`.

```lisp
;;;; unify.lisp --- Logic variables, neural predicates, and unification.

(in-package :nsk)

;;; Logic variables are interned by name so that two occurrences of the same
;;; name share one object. This lets ASSOC use structure equality safely and
;;; keeps printed output readable.

(defvar *logic-vars* (make-hash-table :test 'eq)
  "Interns logic variables by name.")

(defstruct (logic-var (:constructor %make-logic-var (name))
                      (:predicate logic-var-p)
                      (:copier nil))
  (name nil :read-only t))

(defun logic-var (name)
  "Return the canonical logic variable named NAME (a symbol)."
  (or (gethash name *logic-vars*)
      (setf (gethash name *logic-vars*) (%make-logic-var name))))

(defmethod print-object ((v logic-var) stream)
  (format stream "?~a" (logic-var-name v)))

;;; A neural predicate marks a relation that should fall back to the language
;;; model when no exact symbolic match exists.

(defstruct (neural-predicate (:constructor %make-neural-predicate (name))
                             (:predicate neural-predicate-p)
                             (:copier nil))
  (name nil :read-only t))

(defun neural-predicate (name)
  "Wrap NAME as a neural (LLM fallback) predicate."
  (%make-neural-predicate name))

(defmethod print-object ((p neural-predicate) stream)
  (format stream "~~~a" (neural-predicate-name p)))

;;; Unification, in the Norvig/PAIP style. The environment is an alist of
;;; (logic-var . value). +FAIL+ is a distinct sentinel so that NIL can serve
;;; as the empty (successful) environment.

(defconstant +fail+ 'fail "Sentinel returned by UNIFY on failure.")

(defun unify (x y &optional (env nil))
  "Unify X and Y under ENV, returning an extended environment or +FAIL+."
  (cond ((eq env +fail+) +fail+)
        ((eql x y) env)
        ((logic-var-p x) (unify-var x y env))
        ((logic-var-p y) (unify-var y x env))
        ((and (consp x) (consp y))
         (unify (cdr x) (cdr y)
                (unify (car x) (car y) env)))
        ((and (stringp x) (stringp y) (string= x y)) env)
        (t +fail+)))

(defun unify-var (var x env)
  "Unify logic variable VAR against X, following any existing binding."
  (let ((binding (assoc var env :test #'equalp)))
    (if binding
        (unify (cdr binding) x env)
        (acons var x env))))

(defun resolve (x env)
  "Replace bound variables in X with their values from ENV, recursively."
  (cond ((logic-var-p x)
         (let ((b (assoc x env :test #'equalp)))
           (if b (resolve (cdr b) env) x)))
        ((consp x) (cons (resolve (car x) env) (resolve (cdr x) env)))
        (t x)))
```

A few points reward a second look.

`unify` short-circuits on `+fail+` in its first clause. That lets you nest
calls: the cons clause unifies the cars first, then feeds that result straight
into the unification of the cdrs. If the car step failed, the cdr step sees
`+fail+` and passes it through untouched.

`unify-var` is where a variable earns a binding. If the variable is free, it
adds a pair with `acons` and returns the longer environment. If the variable
already points at a value, it unifies that value against the new term. This is
what stops a variable from meaning two different things in one query. The tests
check exactly this: once `?x` binds to `:mark`, unifying it against `:jane`
fails.

`resolve` walks a term and swaps each bound variable for its value, following
chains of bindings to the end. The query engine calls it at the finish to turn
an environment into the answer you see.

The `neural-predicate` struct carries no behavior. It is a tag. Wrapping a name
in it is how the reader records that you typed `~:capital` rather than
`:capital`, and the query engine reads that tag later to decide whether the
model may be consulted.

## The triplestore and its durable log

The store keeps every triple in memory for speed and mirrors every change to
disk for durability. On disk the format is plain: one Lisp list per line, an
append-only log of what happened. Here is a sample log after three additions
and one deletion.

```text
(:ADD :MARK :WROTE :NSK)
(:ADD :JANE :WROTE :BOOK)
(:ADD :MARK :CODES-IN :LISP)
(:DEL :JANE :WROTE :BOOK)
```

Nothing in this file is ever rewritten. To change a fact you append a new line.
To rebuild the graph you read the lines in order and apply each one. After
replaying the log above, the graph holds two triples: Jane's was added and then
removed.

In memory the store keeps three views of the same triples. A list preserves
insertion order. Two hash tables index triples by subject and by object, so a
pattern with a known subject or object narrows to a short candidate list in
`O(1)`$ time instead of scanning everything. Here is the complete
`src/store.lisp`.

```lisp
;;;; store.lisp --- The triplestore: in-memory indices plus a disk log.
;;;;
;;;; Triples are (subject predicate object). Two hash tables index triples by
;;;; subject and by object for fast pattern narrowing. State survives restarts
;;;; through an append-only log of s-expressions that is replayed on startup.

(in-package :nsk)

(defparameter *log-path* #p"nsk-graph.log"
  "Default path of the append-only transaction log.")

(defstruct (graph (:constructor %make-graph) (:copier nil))
  (spo (make-hash-table :test 'equalp))   ; subject -> list of triples
  (osp (make-hash-table :test 'equalp))   ; object  -> list of triples
  (triples '())                            ; every live triple, newest first
  (count 0)
  (log-stream nil)
  (log-path nil))

(defvar *graph* nil "The active knowledge graph.")

(defun make-graph ()
  "Create an empty in-memory graph with no backing log."
  (%make-graph))

;;; Indexing primitives (no logging)

(defun %index (graph triple)
  (destructuring-bind (s p o) triple
    (declare (ignore p))
    (push triple (gethash s (graph-spo graph)))
    (push triple (gethash o (graph-osp graph)))
    (push triple (graph-triples graph))
    (incf (graph-count graph)))
  triple)

(defun %unindex (graph triple)
  (destructuring-bind (s p o) triple
    (declare (ignore p))
    (setf (gethash s (graph-spo graph))
          (remove triple (gethash s (graph-spo graph)) :test #'equalp))
    (setf (gethash o (graph-osp graph))
          (remove triple (gethash o (graph-osp graph)) :test #'equalp))
    (setf (graph-triples graph)
          (remove triple (graph-triples graph) :test #'equalp))
    (decf (graph-count graph)))
  triple)

(defun triple-present-p (graph triple)
  (member triple (gethash (first triple) (graph-spo graph)) :test #'equalp))

;;; Logging

(defun %log (graph entry)
  (let ((s (graph-log-stream graph)))
    (when s
      (let ((*package* (find-package :nsk))
            (*print-readably* nil)
            (*print-pretty* nil))
        (prin1 entry s)
        (terpri s)
        (finish-output s)))))

;;; Public mutation API

(defun add-triple (s p o &optional (graph *graph*))
  "Add triple (S P O) to GRAPH and append it to the log. Duplicates are ignored."
  (let ((triple (list s p o)))
    (unless (triple-present-p graph triple)
      (%index graph triple)
      (%log graph (list :add s p o)))
    triple))

(defun remove-triple (s p o &optional (graph *graph*))
  "Remove triple (S P O) from GRAPH and record the deletion in the log."
  (let ((triple (list s p o)))
    (when (triple-present-p graph triple)
      (%unindex graph triple)
      (%log graph (list :del s p o)))
    triple))

(defun all-triples (&optional (graph *graph*))
  "Return every live triple in insertion order."
  (reverse (graph-triples graph)))

(defun triple-count (&optional (graph *graph*))
  (graph-count graph))

;;; Index-driven candidate selection

(defun indexable-p (term)
  "True when TERM is a concrete value usable as an index key."
  (not (or (logic-var-p term) (neural-predicate-p term))))

(defun candidate-triples (graph pattern)
  "Return the triples that could match PATTERN, narrowed by the indices."
  (destructuring-bind (s p o) pattern
    (declare (ignore p))
    (cond ((indexable-p s) (gethash s (graph-spo graph)))
          ((indexable-p o) (gethash o (graph-osp graph)))
          (t (graph-triples graph)))))

;;; Persistence: replay, open, close

(defun apply-log-entry (graph entry)
  (destructuring-bind (op s p o) entry
    (ecase op
      (:add (let ((tr (list s p o)))
              (unless (triple-present-p graph tr) (%index graph tr))))
      (:del (let ((tr (list s p o)))
              (when (triple-present-p graph tr) (%unindex graph tr)))))))

(defun replay-log (graph path)
  "Rebuild GRAPH by replaying the log at PATH in order."
  (with-open-file (in path :direction :input :if-does-not-exist nil
                           :external-format :utf-8)
    (when in
      ;; *read-eval* is disabled so a stray #. in the log cannot run code.
      (let ((*read-eval* nil)
            (*package* (find-package :nsk)))
        (loop for entry = (read in nil :eof)
              until (eq entry :eof)
              do (apply-log-entry graph entry))))))

(defun open-store (&optional (path *log-path*))
  "Open (or create) the store at PATH, replay its log, and keep it open for
   appending. Returns the graph."
  (let ((graph (%make-graph :log-path path)))
    (when (probe-file path)
      (replay-log graph path))
    (setf (graph-log-stream graph)
          (open path :direction :output :if-exists :append
                     :if-does-not-exist :create :external-format :utf-8))
    graph))

(defun close-store (&optional (graph *graph*))
  "Flush and close the log stream backing GRAPH."
  (when (and graph (graph-log-stream graph))
    (finish-output (graph-log-stream graph))
    (close (graph-log-stream graph))
    (setf (graph-log-stream graph) nil))
  graph)
```

Notice the split between indexing and logging. `%index` and `%unindex` only
touch memory. `add-triple` and `remove-triple` change memory and then append
one line to the log. Replay reuses `%index` and `%unindex` through
`apply-log-entry`, so the code that rebuilds from disk and the code that runs a
live command share the same primitives.

`add-triple` ignores duplicates, so adding the same fact twice writes one line,
not two. That keeps the log honest and the count correct. The tests confirm it:
three distinct adds plus one repeat leave a count of three.

Two safety details are worth naming. `%log` binds `*print-readably*` to `nil`
and `*print-pretty*` to `nil` so each entry writes as one clean line.
`replay-log` binds `*read-eval*` to `nil` while reading, so a log file cannot
run code through the `#.` reader macro. A knowledge base you load from disk
should never execute; this one line guarantees it will not.

`candidate-triples` is the payoff of the two indices. Given a pattern, it
chooses the smallest starting set it can. A known subject uses the subject
index. Failing that, a known object uses the object index. Only a pattern with
a variable subject and a variable object scans the full list. A pattern whose
sole concrete term is the predicate also scans the full list, because the store
keeps no predicate index. Adding one is a good exercise, and you will find it
among the practice problems.

## A natural query syntax with reader macros

We want to write queries that read like logic, not like string manipulation.
The goal is this:

```lisp
[?who :wrote :nsk]                          ; one pattern
(ask (?a) [?a :wrote :nsk] [?a :codes-in :lisp])   ; a conjunction
[:mark ~:codes-in ?lang]                    ; ~ may consult the model
```

Common Lisp lets us add this syntax at read time with reader macros. Three
characters get new meanings. A `?` turns the next symbol into a logic variable.
A `~` turns the next form into a neural predicate. Square brackets collect
exactly three terms into a triple pattern. Each of these expands into ordinary
Lisp:

```text
?person      ->  (logic-var 'person)
~:codes-in   ->  (neural-predicate ':codes-in)
[s p o]      ->  (match-triple s p o)
```

The macros live in their own readtable so ordinary source files keep the
standard syntax. The REPL switches the readtable on for you; other code calls
`enable-nsk-syntax`. Here is the complete `src/reader.lisp`.

```lisp
;;;; reader.lisp --- Reader macros for the NSK query syntax.
;;;;
;;;;   ?name           -> (logic-var 'name)          a logic variable
;;;;   ~pred           -> (neural-predicate 'pred)   an LLM fallback relation
;;;;   [s p o]         -> (match-triple s p o)        a triple pattern
;;;;
;;;; The macros live in their own readtable so ordinary source files keep the
;;;; standard syntax. The REPL binds *readtable* to *nsk-readtable*; other code
;;;; can call ENABLE-NSK-SYNTAX to add them to the current readtable.

(in-package :nsk)

(defvar *nsk-readtable* (copy-readtable nil)
  "A readtable that adds ?var, [triple], and ~neural syntax.")

(defun install-nsk-syntax (&optional (rt *readtable*))
  "Install the NSK reader macros into readtable RT and return it."
  ;; ?name -> (logic-var 'name); non-terminating so foo?bar stays one symbol.
  (set-macro-character #\?
    (lambda (stream char)
      (declare (ignore char))
      (list 'logic-var (list 'quote (read stream t nil t))))
    t rt)
  ;; ~pred -> (neural-predicate 'pred)
  (set-macro-character #\~
    (lambda (stream char)
      (declare (ignore char))
      (list 'neural-predicate (list 'quote (read stream t nil t))))
    t rt)
  ;; ] closes a triple exactly like ) closes a list.
  (set-macro-character #\] (get-macro-character #\) nil) nil rt)
  ;; [s p o] -> (match-triple s p o)
  (set-macro-character #\[
    (lambda (stream char)
      (declare (ignore char))
      (let ((triple (read-delimited-list #\] stream t)))
        (unless (= (length triple) 3)
          (error "NSK triple pattern needs exactly three elements: ~s" triple))
        (cons 'match-triple triple)))
    nil rt)
  rt)

(install-nsk-syntax *nsk-readtable*)

(defun enable-nsk-syntax ()
  "Copy the current *readtable* and add NSK syntax to it."
  (setf *readtable* (copy-readtable *readtable*))
  (install-nsk-syntax *readtable*))

(defun nsk-read-from-string (string)
  "Read one form from STRING using the NSK readtable."
  (let ((*readtable* *nsk-readtable*))
    (read-from-string string)))
```

The `?` and `~` macros are non-terminating, the `t` argument to
`set-macro-character`. That means the character keeps a symbol together when it
appears inside one, so a name like `foo?bar` still reads as a single symbol.
Only a `?` at the start of a token triggers the macro.

The bracket pair is a small trick. `]` gets the same reader as `)`, so it
closes a form. `[` reads terms up to the matching `]` with
`read-delimited-list`, checks that it got exactly three, and builds a
`match-triple` call. A pattern with two or four terms signals an error at read
time, before evaluation ever begins.

Each macro produces a form, not a value. `[?who :wrote :nsk]` becomes the list
`(match-triple (logic-var 'who) :wrote :nsk)`. What that form does when
evaluated is the job of the next section.

## The query engine

Now we connect patterns to the store. A single triple pattern yields a list of
environments, one for each triple it matches. A conjunction runs the patterns
left to right and passes each environment forward, so a variable bound by the
first clause carries its value into the next. When a neural predicate finds no
symbolic match, the engine asks the model for the missing object.

Here is the complete `src/query.lisp`.

```lisp
;;;; query.lisp --- Pattern matching, neural fallback, and the ASK macro.
;;;;
;;;; A single triple pattern yields a list of environments (one per match).
;;;; Conjunctions thread environments forward with MAPCAN. When a ~ predicate
;;;; finds no symbolic match, the engine asks the model for the missing object.

(in-package :nsk)

(defun ground-pattern (pattern env)
  "Replace bound variables in PATTERN with their values from ENV."
  (mapcar (lambda (term)
            (if (logic-var-p term)
                (let ((b (assoc term env :test #'equalp)))
                  (if b (cdr b) term))
                term))
          pattern))

(defun match-triple-pattern (pattern env graph)
  "Return every environment that satisfies PATTERN under ENV. A neural
   predicate first tries a plain symbolic match on its bare name, then falls
   back to the model."
  (let* ((pred (second pattern))
         (neuralp (neural-predicate-p pred))
         ;; For symbolic matching, unwrap a neural predicate to its bare name.
         (spat (if neuralp
                   (list (first pattern) (neural-predicate-name pred) (third pattern))
                   pattern))
         (gpat (ground-pattern spat env))
         (results '()))
    (dolist (tr (candidate-triples graph gpat))
      (let ((e (unify spat tr env)))
        (unless (eq e +fail+) (push e results))))
    (when (and (null results) neuralp)
      (let ((e (neural-match gpat spat env)))
        (when (and e (not (eq e +fail+))) (push e results))))
    (nreverse results)))

(defun neural-match (grounded spat env)
  "Resolve a neural predicate by asking the model for the unknown object."
  (destructuring-bind (subject predicate object) grounded
    (declare (ignore object))
    (when (indexable-p subject)                 ; subject must be concrete
      (let ((target (third spat)))
        (when (logic-var-p target)
          (let ((answer (query-neural-fallback subject predicate)))
            (when answer
              (unify target (sanitize-to-keyword answer) env))))))))

(defun prove (patterns env graph)
  "Prove PATTERNS as a conjunction, threading environments forward."
  (if (null patterns)
      (list env)
      (mapcan (lambda (e) (prove (cdr patterns) e graph))
              (match-triple-pattern (car patterns) env graph))))

(defun collect-vars (form &optional acc)
  "Collect the distinct logic variables appearing in FORM."
  (cond ((logic-var-p form)
         (if (member form acc :test #'equalp) acc (cons form acc)))
        ((consp form) (collect-vars (cdr form) (collect-vars (car form) acc)))
        (t acc)))

;;; A query result wraps the raw solutions so the REPL can print a readable
;;; table while callers can still pull the data out with SOLUTIONS.

(defstruct (query-result (:constructor make-query-result (solutions)))
  solutions)

(defmethod print-object ((r query-result) stream)
  (let ((sols (query-result-solutions r)))
    (cond ((null sols) (format stream "#<no solutions>"))
          ((equal sols '(())) (format stream "yes"))
          (t (format stream "~{~a~^~%~}"
                     (mapcar (lambda (sol)
                               (format nil "~{~a=~s~^, ~}"
                                       (loop for (k . v) in sol
                                             append (list (string-downcase (symbol-name k)) v))))
                             sols))))))

(defun solutions (result)
  "Return the raw list of solutions from a query result (or a plain list)."
  (if (query-result-p result) (query-result-solutions result) result))

(defun run-query (patterns result-vars &optional (graph *graph*))
  "Prove PATTERNS and return a QUERY-RESULT binding each of RESULT-VARS."
  (let ((sols (mapcar (lambda (env)
                        (mapcar (lambda (v)
                                  (cons (logic-var-name v) (resolve v env)))
                                result-vars))
                      (prove patterns nil graph))))
    (make-query-result (remove-duplicates sols :test #'equalp :from-end t))))

(defun match-triple (s p o &optional (graph *graph*))
  "Run one triple pattern, returning a QUERY-RESULT for its variables."
  (let* ((pattern (list s p o))
         (vars (reverse (collect-vars pattern))))
    (run-query (list pattern) vars graph)))

(defmacro ask (result-vars &body clauses)
  "Datalog-style query. RESULT-VARS is a list like (?a ?b); each clause is a
   [s p o] triple pattern. Returns a QUERY-RESULT."
  (let ((patterns
          (mapcar (lambda (clause)
                    (unless (and (consp clause) (eq (first clause) 'match-triple))
                      (error "ASK clause is not a [triple] pattern: ~s" clause))
                    (cons 'list (rest clause)))
                  clauses)))
    `(run-query (list ,@patterns) (list ,@result-vars))))
```

Work through the flow with one clause first. `match-triple-pattern` takes a
pattern, an incoming environment, and the graph. It grounds the pattern by
substituting any bound variables, asks the store for candidate triples, and
unifies the pattern against each candidate. Every success adds one environment
to the results.

The conjunction lives in `prove`, and it is short because `mapcan` does the
work. For the first pattern it gets a list of environments. For each of those
it proves the rest of the patterns, and `mapcan` splices the results together.
An empty pattern list means every clause has been proved, so it returns a list
holding the one surviving environment. This is a depth-first search over the
ways the clauses can all hold at once.

The neural hook sits inside `match-triple-pattern`. If the predicate is a
neural predicate and the symbolic search found nothing, `neural-match` runs.
It has strict preconditions: the subject must be concrete, since the model
needs something to reason about, and the object must be the variable we want
filled. Given both, it calls `query-neural-fallback`, turns the model's string
into a keyword, and unifies that keyword with the target variable. A neural
predicate that does match symbolically never calls the model, because stored
facts win.

`run-query` finishes a query. It proves the patterns, then for each surviving
environment it reads off the values of the result variables and pairs each with
its name. `remove-duplicates` collapses identical rows, so two different proofs
of the same answer show once.

The `ask` macro is a thin front end. Each clause has already been read into a
`(match-triple ...)` form by the bracket macro. `ask` checks that shape, peels
off the `match-triple` head, and rebuilds each clause as a plain list of three
terms for `run-query`. It refuses any clause that is not a triple pattern, so a
typo fails at macro-expansion time with a clear message.

The `query-result` struct carries the raw solutions and prints them in a form a
person can read. An empty result prints `#<no solutions>`. A proof with no
result variables, the shape of a yes/no question, prints `yes`. Otherwise each
row prints as `var=value` pairs. The REPL leans on this printer, while code that
needs the data calls `solutions` to get the raw list.

## Self-contained JSON

The neural layer and the REST server both speak JSON, so NSK carries its own
JSON reader and writer. This keeps the core free of dependencies. The writer
takes a tagged Lisp form and the reader returns plain Lisp data.

A request to the model looks like this on the wire:

```json
{"model":"qwen3.5:4b","system":"...","prompt":"...","format":"json","stream":false}
```

The daemon answers with an envelope whose `response` field holds another JSON
string:

```json
{"model":"qwen3.5:4b","response":"{\"result\": \"Tokyo\"}","done":true}
```

So the reader must parse the outer object, pull out `response`, and parse that
string again to reach `{"result": "Tokyo"}`. The reader returns an object as an
alist of `(string-key . value)`, an array as a list, and the three literals as
`:true`, `:false`, and `:null`. Here is the complete `src/json.lisp`.

```lisp
;;;; json.lisp --- A small, self-contained JSON reader and writer.
;;;;
;;;; NSK keeps its own JSON code so the core has no external dependencies and
;;;; can load and run under a bare LispWorks image. The writer takes a tagged
;;;; Lisp form; the reader returns alists for objects and lists for arrays.

(in-package :nsk)

;;; Writer

(defun json-write-string (string stream)
  (write-char #\" stream)
  (loop for ch across string do
    (case ch
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (#\Newline (write-string "\\n" stream))
      (#\Return (write-string "\\r" stream))
      (#\Tab (write-string "\\t" stream))
      (#\Backspace (write-string "\\b" stream))
      (#\Page (write-string "\\f" stream))
      (t (if (< (char-code ch) #x20)
             (format stream "\\u~4,'0x" (char-code ch))
             (write-char ch stream)))))
  (write-char #\" stream))

(defun json-encode (value &optional stream)
  "Encode VALUE as JSON. Objects are (:object (key . val) ...); arrays are
   (:array val ...); literals are :true, :false, :null. With no STREAM,
   return a string."
  (if stream
      (%json-encode value stream)
      (with-output-to-string (s) (%json-encode value s))))

(defun %json-encode (value stream)
  (cond
    ((stringp value) (json-write-string value stream))
    ((integerp value) (princ value stream))
    ((floatp value) (format stream "~f" value))
    ((eq value :true) (write-string "true" stream))
    ((eq value :false) (write-string "false" stream))
    ((eq value :null) (write-string "null" stream))
    ((and (consp value) (eq (car value) :object))
     (write-char #\{ stream)
     (loop for (pair . more) on (cdr value) do
       (json-write-string (string (car pair)) stream)
       (write-char #\: stream)
       (%json-encode (cdr pair) stream)
       (when more (write-char #\, stream)))
     (write-char #\} stream))
    ((and (consp value) (eq (car value) :array))
     (write-char #\[ stream)
     (loop for (v . more) on (cdr value) do
       (%json-encode v stream)
       (when more (write-char #\, stream)))
     (write-char #\] stream))
    (t (json-write-string (princ-to-string value) stream))))

;;; Reader

(define-condition json-error (error)
  ((message :initarg :message :reader json-error-message))
  (:report (lambda (c s) (format s "JSON parse error: ~a" (json-error-message c)))))

(defstruct (json-cursor (:constructor make-json-cursor (string)))
  (string "" :type string)
  (pos 0 :type fixnum))

(defun jc-peek (c)
  (let ((s (json-cursor-string c)) (p (json-cursor-pos c)))
    (when (< p (length s)) (char s p))))

(defun jc-next (c)
  (prog1 (jc-peek c) (incf (json-cursor-pos c))))

(defun jc-skip-ws (c)
  (loop for ch = (jc-peek c)
        while (and ch (member ch '(#\Space #\Tab #\Newline #\Return)))
        do (jc-next c)))

(defun json-parse (string)
  "Parse a JSON document into Lisp data. Objects become alists of
   (string-key . value); arrays become lists; strings stay strings; numbers
   parse to numbers; true/false/null become :true/:false/:null."
  (let ((c (make-json-cursor string)))
    (prog1 (json-parse-value c) (jc-skip-ws c))))

(defun json-parse-value (c)
  (jc-skip-ws c)
  (let ((ch (jc-peek c)))
    (cond ((null ch) (error 'json-error :message "unexpected end of input"))
          ((char= ch #\{) (json-parse-object c))
          ((char= ch #\[) (json-parse-array c))
          ((char= ch #\") (json-parse-string c))
          ((or (digit-char-p ch) (char= ch #\-)) (json-parse-number c))
          ((char= ch #\t) (json-parse-literal c "true" :true))
          ((char= ch #\f) (json-parse-literal c "false" :false))
          ((char= ch #\n) (json-parse-literal c "null" :null))
          (t (error 'json-error :message (format nil "unexpected char ~a" ch))))))

(defun json-parse-literal (c text value)
  (loop for expected across text
        for got = (jc-next c)
        unless (and got (char= got expected))
          do (error 'json-error :message (format nil "bad literal, expected ~a" text)))
  value)

(defun json-parse-object (c)
  (jc-next c)                           ; consume {
  (jc-skip-ws c)
  (if (eql (jc-peek c) #\})
      (progn (jc-next c) '())
      (let ((pairs '()))
        (loop
          (jc-skip-ws c)
          (let ((key (json-parse-string c)))
            (jc-skip-ws c)
            (unless (eql (jc-next c) #\:)
              (error 'json-error :message "expected : after object key"))
            (push (cons key (json-parse-value c)) pairs))
          (jc-skip-ws c)
          (let ((ch (jc-next c)))
            (cond ((eql ch #\,) nil)
                  ((eql ch #\}) (return (nreverse pairs)))
                  (t (error 'json-error :message "expected , or } in object"))))))))

(defun json-parse-array (c)
  (jc-next c)                           ; consume [
  (jc-skip-ws c)
  (if (eql (jc-peek c) #\])
      (progn (jc-next c) '())
      (let ((items '()))
        (loop
          (push (json-parse-value c) items)
          (jc-skip-ws c)
          (let ((ch (jc-next c)))
            (cond ((eql ch #\,) nil)
                  ((eql ch #\]) (return (nreverse items)))
                  (t (error 'json-error :message "expected , or ] in array"))))))))

(defun json-parse-string (c)
  (unless (eql (jc-next c) #\")
    (error 'json-error :message "expected a string"))
  (let ((out (make-string-output-stream)))
    (loop for ch = (jc-next c) do
      (cond ((null ch) (error 'json-error :message "unterminated string"))
            ((char= ch #\") (return))
            ((char= ch #\\)
             (let ((esc (jc-next c)))
               (case esc
                 (#\" (write-char #\" out))
                 (#\\ (write-char #\\ out))
                 (#\/ (write-char #\/ out))
                 (#\b (write-char #\Backspace out))
                 (#\f (write-char #\Page out))
                 (#\n (write-char #\Newline out))
                 (#\r (write-char #\Return out))
                 (#\t (write-char #\Tab out))
                 (#\u (write-char (code-char (json-parse-hex c 4)) out))
                 (t (error 'json-error :message "bad string escape")))))
            (t (write-char ch out))))
    (get-output-stream-string out)))

(defun json-parse-hex (c n)
  (let ((val 0))
    (dotimes (i n val)
      (let ((d (digit-char-p (jc-next c) 16)))
        (unless d (error 'json-error :message "bad \\u escape"))
        (setf val (+ (* val 16) d))))))

(defun json-parse-number (c)
  (let ((start (json-cursor-pos c)))
    (when (eql (jc-peek c) #\-) (jc-next c))
    (loop for ch = (jc-peek c)
          while (and ch (or (digit-char-p ch) (member ch '(#\. #\e #\E #\+ #\-))))
          do (jc-next c))
    (let ((token (subseq (json-cursor-string c) start (json-cursor-pos c))))
      (if (find-if (lambda (ch) (member ch '(#\. #\e #\E))) token)
          (let ((*read-default-float-format* 'double-float)
                (*read-eval* nil))
            (read-from-string token))
          (parse-integer token)))))

(defun json-get (object key &optional default)
  "Look up KEY (a string) in an object alist produced by JSON-PARSE."
  (let ((pair (and (listp object) (assoc key object :test #'string=))))
    (if pair (cdr pair) default)))
```

The reader is a hand-written recursive descent parser over a small cursor
struct. `json-parse-value` looks at the first character and dispatches: a brace
starts an object, a bracket an array, a quote a string, a digit or minus a
number, and the letters `t`, `f`, or `n` a literal. Each parser consumes its
own closing token and leaves the cursor ready for the next value.

`json-get` is the accessor the rest of the code uses. Give it a parsed object
and a string key and it returns the value or a default. The neural layer calls
it to reach into the model's reply.

Two details keep the code safe and correct. The number parser binds
`*read-eval*` to `nil` before it calls `read-from-string` on a float token, so
a crafted number cannot run code. And the writer's final clause prints any
value it does not recognize as a JSON string through `princ-to-string`, so a
keyword or symbol that slips in becomes a quoted string rather than an error.

## The neural fallback layer

This layer turns a missing fact into a question for a language model. It talks
to a local Ollama daemon over HTTP, sends a strict prompt that demands JSON,
and converts the reply into a keyword the graph can store. The same layer reads
free text into triples for the `:ingest` command.

The engine keeps no hard dependency on an HTTP library. If Dexador is loaded it
uses that. Otherwise, on LispWorks, it opens a raw socket and writes the HTTP
request by hand. So the neural layer works on a stock LispWorks image with
nothing added.

The inference prompt asks the model for one object and constrains the reply to
JSON of the form `{"result": "value"}`. The extraction prompt asks for a list
of triples as `{"triples": [{"subject": "..", "predicate": "..", "object": ".."}]}`.
Here is the complete `src/neural.lisp`.

```lisp
;;;; neural.lisp --- The neural integration layer (local Ollama daemon).
;;;;
;;;; When a symbolic query fails on a ~ predicate, NSK asks the model to infer
;;;; the missing object. The same layer turns free text into triples. HTTP goes
;;;; through dexador when it is loaded, otherwise through a native LispWorks
;;;; socket, so the core keeps no hard dependency on an HTTP library.

(in-package :nsk)

(defparameter *ollama-url* "http://localhost:11434"
  "Base URL of the local Ollama daemon.")

(defparameter *ollama-model* "qwen3.5:4b"
  "Model used for inference and text extraction.")

(defparameter *ollama-timeout* 60
  "Socket timeout, in seconds, for Ollama requests.")

(defparameter *inference-system*
  "You are a graph database inference node. Given a Subject and a Predicate, infer the single most likely Object. Reply ONLY as JSON: {\"result\": \"value\"}."
  "System prompt that constrains inference output to strict JSON.")

(defparameter *extraction-system*
  "You extract knowledge-graph triples from text. Reply ONLY as JSON of the form {\"triples\": [{\"subject\": \"..\", \"predicate\": \"..\", \"object\": \"..\"}]}. Use short lower-case tokens."
  "System prompt that constrains extraction output to strict JSON.")

;;; Term helpers

(defun term-label (term)
  "Readable label for a subject or predicate term."
  (cond ((neural-predicate-p term) (term-label (neural-predicate-name term)))
        ((keywordp term) (string-downcase (symbol-name term)))
        ((symbolp term) (string-downcase (symbol-name term)))
        ((stringp term) term)
        (t (princ-to-string term))))

(defun sanitize-to-keyword (string)
  "Convert an LLM string such as \"Common Lisp\" into the keyword :COMMON-LISP."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return #\. #\,) string))
         (clean (substitute #\- #\Space (string-upcase trimmed))))
    (intern clean :keyword)))

;;; HTTP transport

(defun parse-url (url)
  "Return (values host port path) for a simple http URL."
  (let* ((mark (search "://" url))
         (rest (if mark (subseq url (+ mark 3)) url))
         (slash (position #\/ rest))
         (authority (if slash (subseq rest 0 slash) rest))
         (path (if slash (subseq rest slash) "/"))
         (colon (position #\: authority))
         (host (if colon (subseq authority 0 colon) authority))
         (port (if colon (parse-integer authority :start (1+ colon)) 80)))
    (values host port path)))

(defun http-post-json (url body)
  "POST BODY (a JSON string) to URL and return the response body string."
  (let ((dex-post (and (find-package :dexador)
                       (find-symbol "POST" :dexador))))
    (cond
      (dex-post
       (funcall dex-post url :content body
                :headers '(("Content-Type" . "application/json"))))
      ((and (find-package :comm) (find-symbol "OPEN-TCP-STREAM" :comm))
       (native-http-post-json url body))
      (t (error "No HTTP client available; load dexador or run on LispWorks.")))))

(defun native-http-post-json (url body)
  "POST using a raw LispWorks TCP socket. Resolved dynamically so this file
   compiles without the COMM package present."
  (let ((open-fn (find-symbol "OPEN-TCP-STREAM" :comm))
        (crlf (coerce (list #\Return #\Linefeed) 'string)))
    (multiple-value-bind (host port path) (parse-url url)
      (let ((stream (funcall open-fn host port
                             :read-timeout *ollama-timeout*
                             :element-type 'base-char)))
        (unless stream (error "Cannot connect to ~a:~a" host port))
        (unwind-protect
             (progn
               (write-string (format nil "POST ~a HTTP/1.1~a" path crlf) stream)
               (write-string (format nil "Host: ~a:~a~a" host port crlf) stream)
               (write-string (format nil "Content-Type: application/json~a" crlf) stream)
               (write-string (format nil "Content-Length: ~a~a" (length body) crlf) stream)
               (write-string (format nil "Connection: close~a~a" crlf crlf) stream)
               (write-string body stream)
               (force-output stream)
               (read-http-body stream))
          (close stream))))))

(defun read-http-body (stream)
  "Read an HTTP response from STREAM and return only the body."
  (read-line stream nil "")             ; status line
  (let ((chunked nil) (length nil))
    (loop for line = (read-line stream nil nil)
          while line
          for trimmed = (string-right-trim '(#\Return) line)
          until (string= trimmed "")
          do (let ((low (string-downcase trimmed)))
               (cond ((and (>= (length low) 18)
                           (string= "transfer-encoding:" low :end2 18)
                           (search "chunked" low))
                      (setf chunked t))
                     ((and (>= (length low) 15)
                           (string= "content-length:" low :end2 15))
                      (setf length (parse-integer low :start 15 :junk-allowed t))))))
    (cond (chunked (read-chunked-body stream))
          (length (read-n-chars stream length))
          (t (read-to-eof stream)))))

(defun read-n-chars (stream n)
  (let* ((buf (make-string n))
         (got (read-sequence buf stream)))
    (subseq buf 0 got)))

(defun read-to-eof (stream)
  (with-output-to-string (out)
    (loop for ch = (read-char stream nil nil)
          while ch do (write-char ch out))))

(defun read-chunked-body (stream)
  (with-output-to-string (out)
    (loop
      (let* ((line (string-right-trim '(#\Return) (read-line stream nil "")))
             (semi (position #\; line))
             (size (parse-integer line :radix 16
                                       :end (or semi (length line))
                                       :junk-allowed t)))
        (when (or (null size) (zerop size)) (return))
        (write-string (read-n-chars stream size) out)
        (read-line stream nil "")))))    ; trailing CRLF after each chunk

;;; Ollama calls

(defun ollama-generate (prompt system)
  "Send a /api/generate request and return the model's raw response string."
  (let* ((payload (json-encode
                   (list :object
                         (cons "model" *ollama-model*)
                         (cons "system" system)
                         (cons "prompt" prompt)
                         (cons "format" "json")
                         (cons "stream" :false))))
         (raw (http-post-json (format nil "~a/api/generate" *ollama-url*) payload))
         (outer (json-parse raw)))
    (json-get outer "response")))

(defun query-neural-fallback (subject predicate)
  "Ask the model to infer the object for (SUBJECT PREDICATE). Return a string,
   or NIL if the daemon is unreachable or gives nothing."
  (handler-case
      (let* ((prompt (format nil "Subject: ~a. Predicate: ~a. What is the Object?"
                             (term-label subject) (term-label predicate)))
             (response (ollama-generate prompt *inference-system*)))
        (when (and response (stringp response))
          (let* ((inner (ignore-errors (json-parse response)))
                 (result (and (consp inner) (json-get inner "result"))))
            (cond ((and result (stringp result) (plusp (length result))) result)
                  ((plusp (length response)) response)
                  (t nil)))))
    (error (e)
      (format *error-output* "~&; neural fallback unavailable: ~a~%" e)
      nil)))

(defun text->triples (text)
  "Use the model to parse TEXT into a list of (S P O) keyword triples."
  (handler-case
      (let* ((response (ollama-generate text *extraction-system*))
             (inner (and response (stringp response) (json-parse response)))
             (rows (and (consp inner) (json-get inner "triples"))))
        (loop for row in rows
              for s = (json-get row "subject")
              for p = (json-get row "predicate")
              for o = (json-get row "object")
              when (and (stringp s) (stringp p) (stringp o))
                collect (list (sanitize-to-keyword s)
                              (sanitize-to-keyword p)
                              (sanitize-to-keyword o))))
    (error (e)
      (format *error-output* "~&; extraction unavailable: ~a~%" e)
      nil)))

(defun ingest-text (text &optional (graph *graph*))
  "Extract triples from TEXT and add them to GRAPH. Return the triples added."
  (let ((triples (text->triples text)))
    (dolist (tr triples triples)
      (add-triple (first tr) (second tr) (third tr) graph))))
```

`sanitize-to-keyword` is the bridge from the model's world of strings to the
graph's world of keywords. It trims stray spaces and punctuation, upcases the
text, turns inner spaces into hyphens, and interns the result as a keyword. So
`"Common Lisp"` becomes `:COMMON-LISP` and `"  Tokyo. "` becomes `:TOKYO`. This
is what lets a free-text answer join the same index as your hand-typed facts.

`ollama-generate` builds the request with the JSON writer, posts it, parses the
envelope, and returns the `response` field. Because the request sets
`"format": "json"`, the daemon constrains the model to emit JSON, and the
`response` field holds that JSON as a string. `query-neural-fallback` parses it
a second time to reach the `result` value, and falls back to the raw response
if the inner shape is missing.

Both public functions wrap their work in `handler-case`. If the daemon is down,
or the reply is malformed, they print one short note to the error stream and
return `nil`. A neural query then simply yields no solutions. The engine never
crashes because a model is offline. The test suite proves this by pointing the
client at a dead port and checking that a `~` query fails cleanly.

The native HTTP code is a compact HTTP/1.1 client. It writes the request line
and headers, sends the body, and reads the response, handling both a
`Content-Length` body and a chunked transfer encoding. It resolves the LispWorks
`comm:open-tcp-stream` through `find-symbol` at call time, so the file compiles
even on an image where that package is absent.

## The interactive REPL

The REPL is a read-eval-print loop with the NSK readtable switched on. So you
can type triple patterns and queries next to ordinary Lisp, and both work. A
keyword form at the start acts as a command: `:facts`, `:count`, `:add`, and so
on. Anything else is evaluated as Lisp. Here is the complete `src/repl.lisp`.

```lisp
;;;; repl.lisp --- The interactive NSK read-eval-print loop.
;;;;
;;;; The loop reads Common Lisp with the NSK readtable active, so triple and
;;;; variable syntax works alongside normal evaluation. Keyword forms act as
;;;; shell commands (:help, :add, :facts, and so on).

(in-package :nsk)

(defparameter *banner*
  "NSK: Neural-Symbolic Knowledge Graph Engine
Type :help for commands, :quit to exit.")

(defparameter *bare-commands* '(:help :quit :exit :facts :count :save)
  "Commands typed as a single keyword.")

(defparameter *list-commands* '(:add :del :ingest)
  "Commands typed as a list whose head is a keyword.")

(defun repl-command-p (form)
  (or (and (keywordp form) (member form *bare-commands*))
      (and (consp form) (keywordp (car form)) (member (car form) *list-commands*))))

(defun print-help ()
  ;; WRITE-STRING, not FORMAT: the help text shows ~ syntax literally, and FORMAT
  ;; would read those tildes as directives.
  (fresh-line)
  (write-string "Commands:
  :help                 show this help
  :facts                list every triple
  :count                show the triple count
  :add s p o            add a triple, e.g. (:add :mark :wrote :nsk)
  :del s p o            remove a triple
  :ingest \"text\"        extract triples from text with the model
  :save                 flush the log to disk
  :quit                 leave the REPL

Queries use the NSK syntax:
  [?who :wrote :nsk]                       one pattern
  (ask (?a) [?a :wrote :nsk] [?a :codes-in :lisp])
  [:mark ~:codes-in ?lang]                 ~ falls back to the model
"))

(defun run-repl-command (form)
  "Run a command FORM. Return :QUIT to leave the loop, otherwise NIL."
  (let ((cmd (if (consp form) (car form) form))
        (args (if (consp form) (cdr form) nil)))
    (case cmd
      ((:quit :exit) :quit)
      (:help (print-help) nil)
      (:count (format t "~&~d triples~%" (triple-count)) nil)
      (:save (when *graph* (finish-output (graph-log-stream *graph*)))
             (format t "~&saved.~%") nil)
      (:facts
       (dolist (tr (all-triples)) (format t "~&  ~{~a~^  ~}~%" tr))
       (format t "~&(~d triples)~%" (triple-count)) nil)
      (:add (destructuring-bind (s p o) args
              (add-triple s p o)
              (format t "~&added ~a ~a ~a~%" s p o)) nil)
      (:del (destructuring-bind (s p o) args
              (remove-triple s p o)
              (format t "~&removed ~a ~a ~a~%" s p o)) nil)
      (:ingest (destructuring-bind (text) args
                 (let ((added (ingest-text text)))
                   (format t "~&ingested ~d triple~:p~%" (length added)))) nil)
      (t (format t "~&unknown command: ~a~%" cmd) nil))))

(defun repl-print (value)
  (if (query-result-p value)
      (format t "~&~a~%" value)
      (format t "~&=> ~s~%" value)))

(defun repl (&optional (graph *graph*))
  "Start the interactive loop against GRAPH (or *GRAPH*)."
  (let ((*graph* (or graph *graph* (make-graph)))
        (*readtable* *nsk-readtable*)
        (*package* (find-package :nsk)))
    (format t "~&~a~%" *banner*)
    (loop
      (format t "~&nsk> ")
      (finish-output)
      (let ((form (handler-case (read *standard-input* nil :eof)
                    (end-of-file () :eof)
                    (error (e)
                      (format t "~&; read error: ~a~%" e)
                      (clear-input)
                      :skip))))
        (cond
          ((eq form :eof) (return))
          ((eq form :skip) nil)
          ((repl-command-p form)
           (when (eq :quit (run-repl-command form)) (return)))
          (t (handler-case (repl-print (eval form))
               (error (e) (format t "~&; error: ~a~%" e)))))))
    (format t "~&Bye.~%")))
```

The loop binds three variables for its duration. `*graph*` is the active graph,
`*readtable*` is the NSK readtable, and `*package*` is the `nsk` package. That
last binding means `:add` and friends read as keywords in the right package and
your bare symbols resolve to the engine's names.

`repl-print` decides how to show a value. A `query-result` prints through its
own printer, so you see `who=:MARK`. Anything else prints after a `=>` arrow, so
`(+ 2 3)` shows `=> 5`. This is the seam that lets one prompt serve both queries
and plain Lisp.

Two `handler-case` forms keep the loop alive. A read error clears the input and
skips to the next prompt. An evaluation error prints a note and returns to the
prompt. A typo or a broken query annoys you for one line; it never drops you out
of the session.

`print-help` uses `write-string` rather than `format` on purpose. The help text
shows the literal `~` of the neural syntax, and `format` would read those
tildes as directives. The comment in the code says as much.

One caveat matters for persistence. `(repl)` with no graph makes a fresh
in-memory graph with no log, so a session started that way does not save. To
persist you open a store first, which the standalone binary does for you. The
running section shows both paths.

## The optional REST server

The `--serve` flag turns NSK into a small web service. It exposes `POST /query`
for pattern queries and `GET /health` for status. The server uses Hunchentoot,
but only through `find-symbol` at call time, and it loads the library on demand
through Quicklisp. So this file compiles and loads on a bare image, and the
dependency appears only when you actually start the server. Here is the complete
`src/server.lisp`.

```lisp
;;;; server.lisp --- Optional REST server (Hunchentoot) for the --serve flag.
;;;;
;;;; Hunchentoot is referenced only through FIND-SYMBOL at call time, so this
;;;; file compiles and loads under a bare image. START-SERVER loads the library
;;;; on demand through Quicklisp.

(in-package :nsk)

(defvar *acceptor* nil "The running Hunchentoot acceptor, if any.")

(defun ensure-hunchentoot ()
  "Make sure the HUNCHENTOOT package is loaded; load it via Quicklisp if not."
  (unless (find-package :hunchentoot)
    (let ((quickload (and (find-package :ql) (find-symbol "QUICKLOAD" :ql))))
      (unless quickload
        (error "Quicklisp is not available to load hunchentoot."))
      (funcall quickload :hunchentoot :silent t)))
  (or (find-package :hunchentoot)
      (error "Could not load hunchentoot.")))

(defun hsym (name)
  "Resolve an exported HUNCHENTOOT symbol by NAME at call time."
  (or (find-symbol (string name) :hunchentoot)
      (error "hunchentoot symbol ~a not found" name)))

(defun set-content-type-json ()
  (funcall (fdefinition (list 'setf (hsym "CONTENT-TYPE*"))) "application/json"))

;;; Request fields map to graph terms: "?x" is a variable, null means "any",
;;; and any other string becomes a keyword.

(defun field->term (value role)
  (cond ((or (null value) (eq value :null)) (logic-var role))
        ((and (stringp value) (plusp (length value)) (char= (char value 0) #\?))
         (logic-var (intern (string-upcase (subseq value 1)) :keyword)))
        ((stringp value) (sanitize-to-keyword value))
        (t value)))

(defun term->json (term)
  (cond ((keywordp term) (string-downcase (symbol-name term)))
        ((stringp term) term)
        ((logic-var-p term) (format nil "?~a" (logic-var-name term)))
        (t (princ-to-string term))))

(defun solution->json (solution)
  (cons :object
        (mapcar (lambda (binding)
                  (cons (string-downcase (symbol-name (car binding)))
                        (term->json (cdr binding))))
                solution)))

(defun handle-query ()
  "POST /query with a JSON body {\"subject\":..,\"predicate\":..,\"object\":..}.
   Null or \"?name\" fields are variables. Returns the matching solutions."
  (set-content-type-json)
  (let* ((raw (funcall (hsym "RAW-POST-DATA") :force-text t))
         (request (and raw (ignore-errors (json-parse raw))))
         (s (field->term (json-get request "subject") :subject))
         (p (field->term (json-get request "predicate") :predicate))
         (o (field->term (json-get request "object") :object))
         (sols (solutions (match-triple s p o))))
    (json-encode
     (list :object
           (cons "count" (length sols))
           (cons "results" (cons :array (mapcar #'solution->json sols)))))))

(defun handle-health ()
  "GET /health returns basic engine status."
  (set-content-type-json)
  (json-encode (list :object
                     (cons "status" "ok")
                     (cons "triples" (triple-count))
                     (cons "model" *ollama-model*))))

(defun start-server (&optional (port 8800))
  "Load Hunchentoot if needed and start serving /query and /health on PORT."
  (ensure-hunchentoot)
  (when *acceptor* (stop-server))
  (let ((table (find-symbol "*DISPATCH-TABLE*" :hunchentoot))
        (prefix (hsym "CREATE-PREFIX-DISPATCHER")))
    (set table (list (funcall prefix "/query" 'handle-query)
                     (funcall prefix "/health" 'handle-health))))
  (setf *acceptor* (make-instance (hsym "EASY-ACCEPTOR") :port port))
  (funcall (hsym "START") *acceptor*)
  *acceptor*)

(defun stop-server ()
  "Stop the running acceptor, if any."
  (when *acceptor*
    (funcall (hsym "STOP") *acceptor*)
    (setf *acceptor* nil))
  t)
```

`field->term` is the rule that maps a JSON request field to a query term. A
`null` field, or a missing one, becomes a fresh variable named for its role, so
the server treats it as "any value". A string that starts with `?` becomes a
variable too. Any other string becomes a keyword through the same sanitizer the
neural layer uses. So a request naming a concrete subject and predicate with a
null object asks "for this subject and predicate, what objects are stored?".

`handle-query` reads the body, builds the three terms, runs a single
`match-triple`, and encodes the solutions. Each result object lists only the
fields that were variables, since those are the ones the query bound. A request
with a null object returns objects like `{"object": "nsk"}`, one per match.

`handle-health` reports the live triple count and the configured model name,
which is a cheap way for a caller to confirm the service is up and see how many
facts it holds.

## Command-line entry point and builds

`main` ties the pieces together for a standalone program. It parses flags,
opens the store so the session persists, and then either starts the server or
drops into the REPL. Here is the complete `src/main.lisp`.

```lisp
;;;; main.lisp --- Command-line entry point and argument parsing.

(in-package :nsk)

(defparameter *version* "1.0.0")

(defun version ()
  (format nil "NSK ~a" *version*))

(defun quit-lisp (&optional (code 0))
  #+lispworks (funcall (find-symbol "QUIT" :lispworks) :status code)
  #+sbcl (funcall (find-symbol "EXIT" :sb-ext) :code code)
  #-(or lispworks sbcl) (progn code (values)))

(defun command-line-args ()
  "Return the user-supplied command-line arguments as strings."
  #+lispworks
  (let ((sym (find-symbol "*LINE-ARGUMENTS-LIST*" :system)))
    (if (and sym (boundp sym)) (rest (symbol-value sym)) nil))
  #-lispworks
  (let ((uiop-fn (and (find-package :uiop)
                      (find-symbol "COMMAND-LINE-ARGUMENTS" :uiop))))
    (if uiop-fn (funcall uiop-fn) nil)))

(defun flag-present-p (flag args)
  (member flag args :test #'string=))

(defun flag-value (flag args &optional default)
  (let ((pos (position flag args :test #'string=)))
    (if (and pos (< (1+ pos) (length args)))
        (nth (1+ pos) args)
        default)))

(defun print-usage ()
  (format t "~&~a

Usage: nsk [options]

  (no options)     start the interactive REPL
  --serve          start the REST server instead of the REPL
  --port N         server port (default 8800)
  --db PATH        transaction log path (default nsk-graph.log)
  --help           show this message

Wrap the REPL with rlwrap for history and line editing:
  rlwrap nsk
" (version)))

(defun main ()
  "Program entry point. Parse flags, then serve or drop into the REPL."
  (let* ((args (command-line-args))
         (log (or (flag-value "--db" args) (namestring *log-path*)))
         (*log-path* (pathname log)))
    (when (flag-present-p "--help" args)
      (print-usage)
      (quit-lisp 0))
    (handler-case
        (progn
          (setf *graph* (open-store *log-path*))
          (cond
            ((flag-present-p "--serve" args)
             (let ((port (parse-integer (or (flag-value "--port" args) "8800"))))
               (start-server port)
               (format t "~&~a serving on http://localhost:~a  (Ctrl-C to stop)~%"
                       (version) port)
               (loop (sleep 3600))))
            (t (repl))))
      (error (e)
        (format *error-output* "~&fatal: ~a~%" e)))
    (close-store *graph*)
    (quit-lisp 0)))
```

`main` reads the log path from `--db` or falls back to the default, then calls
`open-store`, which replays any existing log and keeps the file open for
appending. This is the difference between the binary and a bare `(repl)`: the
binary always runs against a real store, so every add and delete lands on disk.
When the loop ends, `main` closes the store and exits with status zero.

The `#+lispworks` and `#+sbcl` reader conditionals let one file target both
compilers. Argument access, quitting, and the build step each have two
spellings, chosen at read time.

Two files support loading and building. `load.lisp` loads the sources into a
running image with no ASDF cache, which is how you develop.

```lisp
;;;; load.lisp --- Load NSK into a running lw image for development.
;;;;
;;;; Usage:
;;;;   echo '(progn (load "load.lisp") (nsk:repl))' | lw
;;;;
;;;; This loads the source files directly (compiling in memory), which needs no
;;;; ASDF cache on disk. For a production build see nsk.asd and build.lisp.

(in-package :cl-user)

(defparameter *nsk-source-files*
  '("packages" "json" "unify" "store" "reader" "neural" "query" "repl" "server" "main"))

(let ((src (merge-pathnames
            "src/"
            (make-pathname :name nil :type nil
                           :defaults (or *load-truename* *load-pathname*)))))
  ;; One compilation unit defers undefined-function reports to the end, so a
  ;; forward reference between these files stays quiet. A missing function still
  ;; warns. This keeps SBCL as quiet as LispWorks on load.
  (with-compilation-unit (:override t)
    (dolist (name *nsk-source-files*)
      (load (merge-pathnames (concatenate 'string name ".lisp") src)))))

(format t "~&NSK loaded. Try (nsk:repl), or load tests/tests.lisp to run tests.~%")
```

The load order matters and matches the dependency stack: `packages` first, then
`json` and `unify`, then `store`, `reader`, `neural`, `query`, and finally the
`repl`, `server`, and `main`. The whole set loads inside one
`with-compilation-unit` so a forward reference between files, for example the
reader mentioning `match-triple` before `query` defines it, does not warn.

`build.lisp` writes the standalone `nsk` binary. It loads the sources, then
calls the compiler's delivery step.

```lisp
;;;; build.lisp --- Build the standalone `nsk` executable.
;;;;
;;;; LispWorks:
;;;;   lw -build build.lisp
;;;;   (produces ./nsk; needs a LispWorks with delivery)
;;;;
;;;; SBCL:
;;;;   sbcl --script build.lisp
;;;;   (produces ./nsk via save-lisp-and-die)

(in-package :cl-user)

(load (merge-pathnames "load.lisp"
                       (or *load-truename* *load-pathname*)))

#+lispworks
(progn
  ;; DELIVER writes a standalone console application. :multiprocessing is on so
  ;; hunchentoot can run under --serve.
  (funcall (find-symbol "DELIVER" :lispworks)
           'nsk:main "nsk" 0
           :console t
           :multiprocessing t
           :keep-symbols t))

#+sbcl
(sb-ext:save-lisp-and-die "nsk"
                          :toplevel #'nsk:main
                          :executable t
                          :compression t)
```

For a Quicklisp or ASDF build there is also `nsk.asd`, which lists the same
files in the same serial order. Any of the three routes, `load.lisp`,
`build.lisp`, or `ql:quickload`, produces the same engine.

## Running NSK

### A development session

Start LispWorks in the project directory, load the engine, and enter the REPL.
Output from the compiler is trimmed here for clarity.

```text
$ lw
CL-USER 1 > (load "load.lisp")
NSK loaded. Try (nsk:repl), or load tests/tests.lisp to run tests.
CL-USER 2 > (nsk:repl)
NSK: Neural-Symbolic Knowledge Graph Engine
Type :help for commands, :quit to exit.
nsk> (:add :mark :wrote :nsk)
added MARK WROTE NSK
nsk> (:add :jane :wrote :book)
added JANE WROTE BOOK
nsk> (:add :mark :codes-in :lisp)
added MARK CODES-IN LISP
nsk> :facts
  MARK  WROTE  NSK
  JANE  WROTE  BOOK
  MARK  CODES-IN  LISP
(3 triples)
nsk> [?who :wrote :nsk]
who=:MARK
nsk> (ask (?a) [?a :wrote :nsk] [?a :codes-in :lisp])
a=:MARK
nsk> (ask () [:jane :wrote :book])
yes
nsk> [?who :wrote :manual]
#<no solutions>
nsk> (+ 2 3)
=> 5
nsk> :quit
Bye.
```

### A neural fallback

With the Ollama daemon running and the `qwen3.5:4b` model pulled, a `~`
predicate can answer a fact you never stored. The graph holds nothing about
Japan, yet the query returns Tokyo:

```text
nsk> [:japan ~:capital ?city]
city=:TOKYO
```

If the daemon is not running, the same query reports the outage and returns no
solutions rather than failing:

```text
nsk> [:japan ~:capital ?city]
; neural fallback unavailable: Cannot connect to localhost:11434
#<no solutions>
```

The extraction path reads free text into triples. This too needs the daemon:

```text
nsk> (:ingest "Ada Lovelace wrote the first program.")
ingested 1 triple
nsk> :facts
  MARK  WROTE  NSK
  JANE  WROTE  BOOK
  MARK  CODES-IN  LISP
  ADA-LOVELACE  WROTE  FIRST-PROGRAM
(4 triples)
```

### Persistence and the REST server

A development REPL started with `(nsk:repl)` runs in memory and does not save.
The standalone binary opens a store, so it persists. Build it, add a fact, and
quit:

```text
$ sbcl --script build.lisp     # writes ./nsk
$ ./nsk
NSK: Neural-Symbolic Knowledge Graph Engine
Type :help for commands, :quit to exit.
nsk> (:add :mark :wrote :nsk)
added MARK WROTE NSK
nsk> :quit
Bye.
```

The log now holds one line:

```text
$ cat nsk-graph.log
(:ADD :MARK :WROTE :NSK)
```

Start the server against that same log and query it over HTTP:

```text
$ ./nsk --serve --port 8800
NSK 1.0.0 serving on http://localhost:8800  (Ctrl-C to stop)
```

From another terminal:

```text
$ curl -s http://localhost:8800/query \
    -H 'Content-Type: application/json' \
    -d '{"subject": null, "predicate": "wrote", "object": "nsk"}'
{"count":1,"results":[{"subject":"mark"}]}

$ curl -s http://localhost:8800/health
{"status":"ok","triples":1,"model":"qwen3.5:4b"}
```

## Interpreting the results

Each line of output above ties back to the theory.

`who=:MARK` is a symbolic answer. The pattern `[?who :wrote :nsk]` unified
against every stored triple. Only `(:mark :wrote :nsk)` matched, binding `?who`
to `:mark`. Jane wrote a book, not NSK, so she did not appear. The engine
returned exactly what you stored, no more.

`a=:MARK` from the `ask` join shows the conjunction at work. The first clause
found two authors, Mark and Jane. The second clause, `[?a :codes-in :lisp]`,
kept only the binding where that same author also codes in Lisp. Mark survived,
Jane did not. The shared variable `?a` is what links the two clauses; `prove`
carried its binding from the first clause into the second.

`yes` answered a query with no result variables, `(ask () [:jane :wrote :book])`.
There was nothing to report back, only a fact to confirm, so the printer says
`yes`. Had the fact been absent, you would have seen `#<no solutions>`, which is
what `[?who :wrote :manual]` returned. In symbolic terms, absence of proof is a
plain "no".

`=> 5` is a reminder that the prompt is a full Lisp REPL. The query syntax sits
beside ordinary evaluation, not on top of it.

`city=:TOKYO` is the neural-symbolic idea in one line. The symbolic search for
`(:japan :capital ?city)` found nothing, because no such triple exists. The `~`
mark permitted a fallback, so the engine asked the model, which answered
"Tokyo", and `sanitize-to-keyword` turned that into `:TOKYO`. The value now
looks exactly like a stored fact and could join further queries. The line
between the two kinds of knowledge is the tilde, and nothing else.

The offline case, `; neural fallback unavailable ...` followed by
`#<no solutions>`, shows the safety property. A missing model degrades the
neural predicate to an ordinary one that happens to have no match. The engine
keeps running.

The REST response `{"count":1,"results":[{"subject":"mark"}]}` deserves a close
read. The request set the predicate and object to concrete values and left the
subject null. The server read that null as a variable, so the one bound field
in each result is `subject`. The concrete fields do not repeat in the results,
because a query returns the values of its unknowns. The `/health` reply confirms
the server loaded the persisted log: one triple, matching the single line in
`nsk-graph.log`.

## Testing the engine

The suite in `tests/tests.lisp` exercises every layer without a live daemon. It
uses a tiny `check` macro that counts passes and failures and prints one line
per assertion. Run it with:

```text
$ echo '(progn (load "load.lisp") (load "tests/tests.lisp"))' | lw
```

The output walks through the sections and ends with a tally:

```text
== unification ==
  ok   (EQ +FAIL+ (UNIFY :A :B))
  ok   (NOT (EQ +FAIL+ (UNIFY :A :A)))
  ok   variable binds to value
  ok   logic vars intern by name
  ok   an already-bound var will not rebind
  ok   resolve follows a binding

== store ==
  ok   duplicates are ignored
  ok   subject index works
  ok   object index works
  ok   insertion order kept
  ok   remove updates the count
  ok   remove clears the index

  ... sections for persistence, reader macros, query engine,
      json, server helpers, neural fallback, and the REPL ...

== neural fallback (no daemon) ==
  ok   a ~ query fails cleanly when Ollama is down

== repl (scripted) ==
  ok   repl :add reports the addition
  ok   repl runs a single pattern
  ok   repl runs an ask join
  ok   repl :count is correct
  ok   repl mutated the graph

==================================
NSK tests: 41 passed, 0 failed
==================================
```

The persistence section is worth calling out. It opens a store, adds two
triples, removes one, and closes the store. Then it opens the same log again in
a fresh graph and checks the replay: the right count, the surviving triple, and
the fact that the deletion stuck. This is the durability claim, proved against a
real file on disk.

The neural section points the client at a dead port and confirms a `~` query
returns no solutions without error. So you can run the whole suite offline and
still cover the fallback path.

## Wrap up

NSK is small, but it shows a complete idea. Symbolic reasoning gives you exact,
fast answers over the facts you recorded. A language model gives you plausible
answers over the far larger set of facts it read during training. Put the model
behind the symbolic search, gated by a single mark in the syntax, and you get an
engine that prefers what it knows and reaches for a guess only when it must.

Along the way the code showed several Common Lisp techniques worth keeping.
Reader macros gave the query language a clean surface with no parser. A tagged
s-expression log gave durability in a few lines, with `*read-eval*` disabled for
safety. `find-symbol` at call time let the core stay free of Hunchentoot and
Dexador while still using them when present. And unification, the oldest idea
here, turned pattern matching into a dozen lines that the rest of the engine
builds on.

The design leaves clear room to grow. There is no predicate index, no negation,
no way to store a neural answer back into the graph, and no confidence score on
a guess. The practice problems take up several of these.

## Optional practice problems

These build on the code in this chapter. Each names the files you will touch.
Start with the store and query problems; they need no daemon and the test suite
gives you a pattern to copy.

1. **Add a predicate index.** Today `candidate-triples` in `store.lisp` narrows
   by subject or object but scans every triple when only the predicate is known,
   as in `[?s :wrote ?o]`. Add a third hash table, `pso`, keyed by predicate.
   Update `%index`, `%unindex`, and `candidate-triples` to use it. Add a test
   that stores many triples under different predicates and confirms the new
   index returns a short candidate list.

2. **Cache neural answers.** When `neural-match` in `query.lisp` gets an answer
   from the model, add it to the graph as an ordinary triple so the next query
   for the same subject and predicate is a symbolic hit and costs no HTTP call.
   Decide whether the cached triple should use the neural predicate's bare name.
   Confirm with a scripted test that a second identical `~` query does not call
   the model.

3. **Count solutions.** Add a REPL command `(:query-count pattern)` that prints
   how many solutions a pattern has rather than the bindings. Reuse `solutions`
   and `length`. For a stretch, add an `ask`-level aggregate that returns the
   number of distinct rows.

4. **Negation as failure.** Add a clause form `(not [s p o])` to the `ask` macro
   in `query.lisp` that succeeds only when the inner pattern has no solutions.
   Thread the current environment in so the negated pattern sees existing
   bindings. Note the ordering rule: a negated clause should run after the
   variables it mentions are bound.

5. **A facts endpoint.** Add `GET /facts` to `server.lisp` that returns every
   triple as a JSON array of three-element arrays, for example
   `[["mark","wrote","nsk"]]`. Reuse `all-triples` and `term->json`, and add a
   dispatcher entry in `start-server`.

6. **Extend unification to lists and numbers.** The engine treats terms as
   atoms. Store a triple whose object is a list, such as
   `(:mark :knows (:lisp :scheme))`, and confirm that a pattern with a variable
   in that position unifies against the list. The `unify` function already
   recurses into conses, so the work is mostly in the reader and the printer.
   Add tests that bind a variable to a list and resolve it back.

7. **A confidence field on neural answers.** Change the inference prompt in
   `neural.lisp` to ask for `{"result": "value", "confidence": 0.0}` and parse
   the number with `json-get`. Drop any answer below a threshold you choose.
   Decide how the REPL should show a low-confidence result: skip it, or mark it.

8. **Round-trip a saved graph.** Write a script that opens a store, ingests a
   paragraph of text with `ingest-text`, closes the store, reopens it, and
   prints the facts. This checks that model-extracted triples survive a restart
   the same way hand-typed ones do. Run it twice and confirm the log grows only
   by the new facts, since duplicates are ignored.
