- Architecture: A Common Lisp triplestore requires solid persistence. An append-only transaction log of s-expressions is the most idiomatic and robust way to persist state to disk while maintaining a fast in-memory hash table index for O(1) lookups.
- Generative Integration: We need the dexador library to make HTTP calls to the local Ollama daemon. The qwen3.5:4b model will act as our fallback inference engine when strict Datalog-style queries fail. JSON responses will be parsed using jonathan or shasht.
- Command Line Experience: Building a standalone binary in SBCL is trivial using save-lisp-and-die. For the REPL line editing, we have two paths. We can either statically link the linedit Common Lisp package for native history and text rewrapping, or we can build a standard REPL and instruct the user to run rlwrap nsk. I will include both approaches.
- REST Server: The --serve flag requires parsing arguments via UIOP. If detected, we start a hunchentoot instance on port 8800 and expose an HTTP endpoint instead of dropping into the REPL.

NOTE on command line editing: it is OK to provide README.md instructions for wrapping 'nsk' command line utility in rlwrap

After writing a README.md file, also write a short MANUAL.md doc file.

NSK: Neural-Symbolic Knowledge Graph Engine
1. System Overview
NSK is a hybrid neural-symbolic graph database written in Common Lisp. It bridges deterministic logic queries with probabilistic reasoning by integrating a local generative model via Ollama. The system operates primarily as an interactive REPL but can also run as a headless REST service for external applications.
2. Core Requirements
Language: Common Lisp (SBCL recommended for native binary compilation).
AI Backend: Ollama running the qwen3.5:4b model locally for entity extraction and semantic fallback queries.
Storage: Graph data must be strictly persisted to disk to prevent data loss.
Interface: A command-line executable named nsk.
Interactivity: A custom REPL supporting line editing and command history.
Networking: An optional --serve flag to expose a REST API on port 8800.
3. Architecture Design
3.1. Storage and Data Model Architecture
The knowledge graph functions as a robust triplestore utilizing a Subject, Predicate, Object structure.
In-Memory Graph: To maximize query performance, triples are loaded into Lisp hash tables indexed by both Subject and Object. This dual indexing guarantees O(1) traversal times for complex pattern matching.
Disk Persistence: State persistence relies on an append-only transaction log. Every graph mutation is immediately serialized and flushed to disk as an s-expression. Upon startup, the NSK engine reads this file sequentially and replays the transaction log to accurately rebuild the entire in-memory graph structure.
3.2. Neural Integration Layer
NSK communicates with the local Ollama daemon via HTTP using the dexador client library.
Model Configuration: All generative requests explicitly target the qwen3.5:4b model.
Inference Pipeline: When the classical logic engine fails to resolve an exact symbolic match, the query is automatically forwarded to the neural layer. The LLM evaluates the surrounding node context to infer missing relationships. Additionally, this layer parses raw, unstructured text inputs into strict formal triples for database insertion.
3.3. Application Entrypoint and Networking
The Lisp image is dumped as a standalone executable.
The startup routine uses uiop:command-line-arguments to parse flags.
If the --serve argument is passed, NSK bypasses the REPL and launches a hunchentoot web server bound to port 8800. This server exposes a /query endpoint that accepts and returns JSON payloads.
3.4. Interactive REPL Experience
When executed without the server flag, NSK boots into an interactive query prompt. The loop parses custom reader macros for graph traversal. To provide a high-quality terminal experience with arrow-key navigation and command history, the binary is designed to be wrapped via rlwrap nsk. For a purely native approach, the linedit library is statically linked during the SBCL build process to handle terminal IO directly.



To make querying the graph feel like a native extension of Common Lisp rather than a bolted-on string query language, we should use reader macros to implement a declarative syntax. The goal is to let you write Datalog-style queries that can seamlessly mix with standard Lisp evaluation and variables.
Here is a design utilizing three specific reader macros to handle variables, strict triples, and neural fallbacks.
1. Logic Variables: The ? Macro
We need a way to distinguish logic variables from standard Lisp symbols. By binding the ? character, we can tell the Lisp reader to wrap the adjacent symbol into a logic-var struct or list.
This keeps the query clean without needing to quote or manually declare every variable.

(set-macro-character #\?
  (lambda (stream char)
    (declare (ignore char))
    ;; Reads the next symbol and wraps it
    `(logic-var ',(read stream t nil t))))

Result: Typing ?name at the REPL is read as (logic-var 'name). During query execution, the engine knows this is an unbound variable that needs unification.

2. Graph Patterns: The [ and ] Macros
To visually separate graph traversals from standard Lisp function calls (which use parentheses), we can bind square brackets to represent a triple pattern. The reader will translate the bracketed list into a call to our graph-matching engine.


;; Define the closing bracket to act like a closing parenthesis
(set-macro-character #\] (get-macro-character #\)))

;; Define the opening bracket to read until the closing bracket
(set-macro-character #\[
  (lambda (stream char)
    (declare (ignore char))
    (let ((triple (read-delimited-list #\] stream t)))
      `(match-triple ,(first triple) ,(second triple) ,(third triple)))))

Result: Typing [?person :wrote :nsk] is read by Lisp as (match-triple (logic-var 'person) :wrote :nsk).



Bringing It Together in the REPL
With these three reader macros in your environment, you can write expressive, highly readable queries that combine logic programming with standard Common Lisp constructs.
For example, to find out who authored a book and then use the LLM to guess their primary programming language, you could write a query macro (e.g., ask) that evaluates multiple bracketed forms:

(ask (?author ?lang)
  [?author :wrote :book-title]
  [?author ~:codes-in ?lang])

The Lisp reader expands this entirely at read-time, meaning the compiler can optimize the resulting match-triple calls before execution. The ask macro handles the unification environment, passing bindings down the chain, and dropping out to the Ollama HTTP client only when the ~ macro flags it.

## unification algorithm 

Chain of Thought Analysis
State Representation: Unification requires tracking state. An association list (alist) is the most idiomatic Common Lisp structure for an environment of variable bindings (e.g., ((?author . :mark) (?lang . :lisp))).
The Unification Primitive: We need a function unify(x, y, env) that attempts to make x and y equal given the current environment, returning a new environment or a failure constant. This is standard Prolog-style unification, popularized in Lisp by Peter Norvig's PAIP.
State Propagation (Chaining): A single graph query (a triple pattern) against the database might have multiple valid answers. Therefore, evaluating a triple returns a list of environments.
Handling Conjuncts: To chain multiple triples, we apply a "flatmap" pattern. We take the list of environments generated by the first triple, and for each environment, we evaluate the second triple. The mapcan function handles this perfectly.
Neural Interception: The ~ macro triggers a fallback. If the standard database lookup yields an empty list of environments, the engine resolves the known variables in the current environment and ships the prompt to Ollama, injecting the LLM's response as a newly synthesized binding.
The Core Unification Engine
To process these logic variables, the engine relies on a binding environment. We start with the fundamental unification algorithm.
1. Variable Binding
The unify function compares two elements and updates the association list. If a variable is already bound, it recursively unifies against the bound value.

(defconstant +fail+ 'fail)

(defun unify (x y &optional (env nil))
  "Unify x and y in the given environment."
  (cond ((eq env +fail+) +fail+)
        ((eql x y) env)
        ((logic-var-p x) (unify-var x y env))
        ((logic-var-p y) (unify-var y x env))
        ((and (consp x) (consp y))
         (unify (cdr x) (cdr y) 
                (unify (car x) (car y) env)))
        (t +fail+)))

(defun unify-var (var x env)
  "Unify a logic variable against a value."
  (let ((binding (assoc var env :test #'equalp)))
    (if binding
        (unify (cdr binding) x env)
        (acons var x env))))


2. Querying the Graph
A single triple pattern (like [?person :wrote :nsk]) is evaluated by iterating over the entire in-memory hash table. For every triple in the database, the engine attempts to unify it with the pattern.
It collects every successful environment into a list.

(defun match-triple-pattern (pattern env database)
  "Returns a list of all valid environments that satisfy the pattern."
  (let ((results nil))
    (loop for db-triple in database do
      (let ((new-env (unify pattern db-triple env)))
        (unless (eq new-env +fail+)
          (push new-env results))))
    results))

3. Chaining the State
This is where the magic happens. To evaluate a complex query like (ask (?author) [?author :wrote :book] [?author :codes-in :lisp]), we must pass the state of the first match into the second match.
Because the first match might return multiple authors, we map the second match over the results of the first using mapcan (which concatenates the resulting lists of environments).

(defun prove-query (patterns env database)
  "Recursively evaluates a list of patterns, passing environments forward."
  (if (null patterns)
      (list env) ; Success: return the final environment
      (let ((first-pattern (car patterns))
            (rest-patterns (cdr patterns)))
        (mapcan (lambda (current-env)
                  (prove-query rest-patterns current-env database))
                (match-triple-pattern first-pattern env database)))))


4. Injecting the Neural Fallback
The neural-predicate (triggered by the ~ macro) hooks directly into match-triple-pattern.
If (match-triple-pattern pattern env database) returns NIL (zero matches), and the predicate is flagged as neural, the engine intercepts the failure. It walks the current env to replace bound variables with their concrete values, constructs a prompt for qwen3.5:4b, and wraps the LLM's JSON response into a new binding environment list to keep the chain alive.

## neural fallback implementation

Chain of Thought Analysis
Variable Resolution: Before sending anything to the LLM, the engine must resolve all known variables in the failed triple pattern using the current environment. If ?author is already bound to :mark, the pattern [?author ~:codes-in ?lang] becomes [:mark ~:codes-in ?lang].
Prompt Engineering for strict output: We must constrain the qwen3.5:4b model to output machine-readable data. The Ollama API supports a format: "json" parameter. We will provide a system prompt that dictates the exact JSON schema.
Sanitization: LLMs generate strings. Lisp symbolic graphs operate on symbols (typically keywords like :common-lisp). We need a deterministic sanitization function to convert the string "Common Lisp" into the keyword :common-lisp before binding it to the environment.
State Re-integration: Once the LLM provides the missing node, we generate a new environment containing the new binding and return it as a single-item list, allowing the mapcan function in prove-query to continue chaining.
Here is how we implement the neural fallback interceptor.
1. Grounding the Pattern
First, we walk the triple to substitute any variables that have already been bound in the environment.

(defun ground-pattern (pattern env)
  "Replaces variables in the pattern with their bound values from the environment."
  (mapcar (lambda (term)
            (if (logic-var-p term)
                (let ((binding (assoc term env :test #'equalp)))
                  (if binding (cdr binding) term))
                term))
          pattern))


For the pattern ((logic-var 'author) (neural-predicate 'codes-in) (logic-var 'lang)) and environment (((logic-var 'author) . :mark)), this returns (:mark (neural-predicate 'codes-in) (logic-var 'lang)).
2. Constructing the Ollama API Call
We define a function that takes the grounded Subject, Predicate, and the name of the unknown variable, and constructs a strict JSON prompt for Ollama.

(defun query-neural-fallback (subject predicate target-var)
  "Calls the local Ollama daemon to infer a missing relationship."
  (let* ((prompt (format nil "Subject: ~a. Predicate: ~a. What is the Object?" 
                         subject predicate))
         (payload (jonathan:to-json
                   `(:|model| "qwen3.5:4b"
                     :|system| "You are a graph database inference node. Reply ONLY in JSON format: {\"result\": \"value\"}."
                     :|prompt| ,prompt
                     :|format| "json"
                     :|stream| nil)))
         (response (dexador:post "http://localhost:11434/api/generate"
                                 :content payload
                                 :headers '(("Content-Type" . "application/json")))))
    
    ;; Parse the JSON response
    (let* ((parsed (jonathan:parse response))
           (response-json (getf parsed :|response|))
           (inner-parsed (jonathan:parse response-json)))
      (getf inner-parsed :|result|))))


3. Sanitizing and Binding the Result
The LLM will return a natural language string like "Common Lisp". To maintain strict typing in our Lisp environment, we must sanitize this string into a canonical keyword before binding it.

(defun sanitize-to-keyword (string)
  "Converts an LLM string into a Lisp keyword (e.g., 'Common Lisp' -> :COMMON-LISP)."
  (let ((clean-string (substitute #\- #\Space (string-upcase string))))
    (intern clean-string "KEYWORD")))


4. The Interceptor Hook
Finally, we modify our previous match-triple-pattern function to catch the failure and execute the fallback.

(defun match-triple-pattern (pattern env database)
  (let ((results nil)
        (grounded (ground-pattern pattern env)))
    
    ;; 1. Try standard symbolic lookup
    (loop for db-triple in database do
      (let ((new-env (unify pattern db-triple env)))
        (unless (eq new-env +fail+)
          (push new-env results))))
    
    ;; 2. Neural Fallback Intercept
    (when (and (null results) 
               (neural-predicate-p (second grounded)))
      (let* ((subject (first grounded))
             (predicate (neural-predicate-name (second grounded)))
             (object (third grounded)))
        
        ;; If the object is the unknown variable, ask the LLM to infer it
        (when (logic-var-p object)
          (let* ((llm-string (query-neural-fallback subject predicate object))
                 (lisp-keyword (sanitize-to-keyword llm-string))
                 ;; Unify the LLM's answer with the unbound variable
                 (neural-env (unify object lisp-keyword env)))
            (unless (eq neural-env +fail+)
              (push neural-env results))))))
              
    results))


his architecture ensures the graph engine remains entirely deterministic until it explicitly hits a failure state on a ~ flagged predicate, at which point it securely bridges the gap using the local inference server.
