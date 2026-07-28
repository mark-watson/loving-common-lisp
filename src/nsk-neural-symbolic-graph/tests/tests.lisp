;;;; tests.lisp --- Test suite for NSK. Run with:
;;;;   echo '(progn (load "load.lisp") (load "tests/tests.lisp"))' | lw

(in-package :nsk)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (form &optional label)
  (let ((name (or label `',form)))
    `(handler-case
         (if ,form
             (progn (incf *pass*) (format t "~&  ok   ~a~%" ,name))
             (progn (incf *fail*) (format t "~&  FAIL ~a~%" ,name)))
       (error (e)
         (incf *fail*)
         (format t "~&  ERR  ~a  <~a>~%" ,name e)))))

(defun section (title) (format t "~&~%== ~a ==~%" title))

;;; ------------------------------------------------------------------
(section "unification")

(check (eq +fail+ (unify :a :b)))
(check (not (eq +fail+ (unify :a :a))))
(check (let ((v (logic-var 'x)))
         (equalp :mark (cdr (assoc v (unify v :mark) :test #'equalp))))
       "variable binds to value")
(check (eq (logic-var 'x) (logic-var 'x)) "logic vars intern by name")
(check (let* ((v (logic-var 'x))
              (env (unify v :mark)))
         (eq +fail+ (unify v :jane env)))
       "an already-bound var will not rebind")
(check (equalp :mark (resolve (logic-var 'x) (unify (logic-var 'x) :mark)))
       "resolve follows a binding")

;;; ------------------------------------------------------------------
(section "store")

(let ((g (make-graph)))
  (add-triple :mark :wrote :nsk g)
  (add-triple :jane :wrote :book g)
  (add-triple :mark :codes-in :lisp g)
  (add-triple :mark :wrote :nsk g)      ; duplicate, ignored
  (check (= 3 (triple-count g)) "duplicates are ignored")
  (check (= 2 (length (gethash :mark (graph-spo g)))) "subject index works")
  (check (= 1 (length (gethash :book (graph-osp g)))) "object index works")
  (check (equalp '(:mark :wrote :nsk) (first (all-triples g))) "insertion order kept")
  (remove-triple :mark :wrote :nsk g)
  (check (= 2 (triple-count g)) "remove updates the count")
  (check (null (member '(:mark :wrote :nsk) (gethash :mark (graph-spo g))
                       :test #'equalp))
         "remove clears the index"))

;;; ------------------------------------------------------------------
(section "persistence (replay)")

(let ((path #p"nsk-test-tmp.log"))
  (ignore-errors (delete-file path))
  (let ((g (open-store path)))
    (add-triple :a :b :c g)
    (add-triple :d :e :f g)
    (remove-triple :a :b :c g)
    (close-store g))
  (let ((g2 (open-store path)))
    (check (= 1 (triple-count g2)) "replay reaches the right count")
    (check (equalp '(:d :e :f) (first (all-triples g2))) "replay keeps live triples")
    (check (null (triple-present-p g2 '(:a :b :c))) "replayed deletion sticks")
    (close-store g2))
  (ignore-errors (delete-file path)))

;;; ------------------------------------------------------------------
(section "reader macros")

(let ((form (nsk-read-from-string "[?person :wrote :nsk]")))
  (check (eq 'match-triple (first form)) "[ ] expands to match-triple")
  (check (equal '(logic-var (quote person)) (second form)) "?x expands to a logic var")
  (check (eq :wrote (third form)))
  (check (eq :nsk (fourth form))))

(let ((form (nsk-read-from-string "[?a ~:codes-in ?l]")))
  (check (equal '(neural-predicate (quote :codes-in)) (third form))
         "~pred expands to a neural predicate"))

;;; ------------------------------------------------------------------
(section "query engine")

(let ((*graph* (make-graph)))
  (add-triple :mark :wrote :nsk)
  (add-triple :jane :wrote :nsk)
  (add-triple :mark :codes-in :lisp)
  ;; single pattern
  (let ((sols (solutions (match-triple (logic-var 'who) :wrote :nsk))))
    (check (= 2 (length sols)) "two authors wrote nsk")
    (check (member :mark sols :key (lambda (s) (cdr (assoc 'who s)))) "mark is one"))
  ;; join across two patterns via ASK
  (let* ((q (nsk-read-from-string
             "(ask (?a) [?a :wrote :nsk] [?a :codes-in :lisp])"))
         (sols (solutions (eval q))))
    (check (= 1 (length sols)) "join narrows to one author")
    (check (eq :mark (cdr (assoc 'a (first sols)))) "the author is mark")))

;;; ------------------------------------------------------------------
(section "json")

(check (string= "\"a\\\"b\"" (json-encode "a\"b")) "strings are escaped")
(check (equalp '(("x" . 1) ("y" . :true)) (json-parse "{\"x\":1,\"y\":true}"))
       "objects parse to alists")
(check (equal '(1 2 3) (json-parse "[1,2,3]")) "arrays parse to lists")
(let* ((inner "{\"result\": \"Common Lisp\"}")
       (outer (json-encode (list :object
                                 (cons "model" "m")
                                 (cons "response" inner)
                                 (cons "done" :true))))
       (parsed (json-parse outer))
       (response (json-get parsed "response"))
       (result (json-get (json-parse response) "result")))
  (check (string= "Common Lisp" result) "extract result from an Ollama-style reply")
  (check (eq :common-lisp (sanitize-to-keyword result)) "sanitize to keyword"))

(check (eq :common-lisp (sanitize-to-keyword "  Common Lisp. ")) "sanitize trims and upcases")

;;; ------------------------------------------------------------------
(section "server helpers")

(check (logic-var-p (field->term :null :subject)) "null field becomes a variable")
(check (logic-var-p (field->term "?who" :subject)) "?who field becomes a variable")
(check (eq :mark (field->term "mark" :subject)) "plain field becomes a keyword")
(check (string= "mark" (term->json :mark)) "keyword renders as lower-case text")
(let ((*graph* (make-graph)))
  (add-triple :mark :wrote :nsk)
  (let* ((sol (first (solutions (match-triple :mark :wrote (logic-var 'object)))))
         (json (json-encode (solution->json sol))))
    (check (search "nsk" json) "solution renders to json")))

;;; ------------------------------------------------------------------
(section "neural fallback (no daemon)")

(let ((*graph* (make-graph))
      (*ollama-url* "http://127.0.0.1:9")   ; nothing listens here
      (*ollama-timeout* 2))
  (add-triple :mark :wrote :nsk)
  (let ((sols (solutions
               (eval (nsk-read-from-string "(ask (?l) [:mark ~:codes-in ?l])")))))
    (check (null sols) "a ~ query fails cleanly when Ollama is down")))

;;; ------------------------------------------------------------------
(section "repl (scripted)")

(let* ((*graph* (make-graph))
       (out (with-output-to-string (*standard-output*)
              (with-input-from-string
                  (*standard-input*
                   "(:add :mark :wrote :nsk)
                    (:add :mark :codes-in :lisp)
                    [?who :wrote :nsk]
                    (ask (?a) [?a :wrote :nsk] [?a :codes-in :lisp])
                    :count")
                (repl *graph*)))))
  (check (search "added" out) "repl :add reports the addition")
  (check (search "who=:MARK" out) "repl runs a single pattern")
  (check (search "a=:MARK" out) "repl runs an ask join")
  (check (search "2 triples" out) "repl :count is correct")
  (check (= 2 (triple-count *graph*)) "repl mutated the graph"))

;;; ------------------------------------------------------------------
(format t "~&~%==================================~%")
(format t "~&NSK tests: ~d passed, ~d failed~%" *pass* *fail*)
(format t "~&==================================~%")
(when (find-package :lispworks)
  (funcall (find-symbol "QUIT" :lispworks) :status (if (zerop *fail*) 0 1)))
