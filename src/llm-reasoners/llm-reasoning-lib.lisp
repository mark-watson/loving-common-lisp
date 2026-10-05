;;;; llm-reasoning-lib.lisp
;;;;
;;;; A small Common Lisp port of the core abstractions of llm-reasoners
;;;; (https://github.com/maitrix-org/llm-reasoners), with the language model
;;;; supplied by the locally installed "litelm" library.
;;;;
;;;; The Python library separates a reasoning method into three pieces:
;;;;
;;;;   LanguageModel   -- talks to the model                       (GENERATE)
;;;;   WorldModel      -- state, dynamics, terminal test   (INIT-STATE, STEP,
;;;;                                                        IS-TERMINAL)
;;;;   SearchConfig    -- what actions to try and how good they are
;;;;                                          (GET-ACTIONS, FAST-REWARD, REWARD)
;;;;
;;;; A SEARCH-ALGORITHM (BEAM-SEARCH is provided) explores the world model using
;;;; the search config, and a REASONER ties the three together.
;;;;
;;;; Chain-of-thought does not need a search at all, so it is provided
;;;; separately as COT-REASONER -- mirroring the Python repo, where CoT is a
;;;; plain class rather than a Reasoner.
;;;;
;;;;   (llm-reasoning:run-self-tests)   ; offline checks; no model required
;;;;
;;;; ---------------------------------------------------------------------------
;;;; NOTE ON DEXADOR AND *PRINT-CASE*
;;;;
;;;; litelm sends its POST body with dexador.  dexador's DEFINE-ALIST-CACHE
;;;; macro builds function names with (FORMAT NIL "LOOKUP-IN-~A" ...), which is
;;;; sensitive to *PRINT-CASE*.  If dexador is compiled while *PRINT-CASE* is
;;;; :DOWNCASE (as this user's ~/.sbclrc sets it), the definitions are created
;;;; with mixed-case names such as |LOOKUP-IN-content-encoding-cache| while the
;;;; source references read as LOOKUP-IN-CONTENT-ENCODING-CACHE -- so any POST
;;;; that carries a body dies with "The function ... is undefined".  GET still
;;;; works, which makes this confusing.
;;;
;;;; We therefore bind *PRINT-CASE* to :UPCASE around the load below.  That is
;;;; enough on a fresh compile; if a bad fasl is already cached, force one
;;;; rebuild:
;;;;
;;;;   (let ((*print-case* :upcase)) (asdf:load-system :dexador :force t))
;;;;
;;;; See the "Gotchas" section of README.md for the full write-up.

(defpackage #:llm-reasoning
  (:use #:cl)
  ;; STEP is a COMMON-LISP macro and SEARCH a COMMON-LISP function; we shadow
  ;; them so the protocol can keep the names llm-reasoners uses.  Packages that
  ;; :USE both CL and this one must shadowing-import those two symbols (see
  ;; CoT-gsm8k-example.lisp, which imports only what it needs instead).
  (:shadow #:step #:search)
  (:export
   ;; language model protocol
   #:language-model
   #:generate
   #:model-name
   ;; world model protocol
   #:world-model
   #:init-state
   #:step
   #:is-terminal
   ;; search config protocol
   #:search-config
   #:get-actions
   #:fast-reward
   #:reward
   #:search-config-reward-alpha
   ;; shared component state
   #:reasoning-component
   #:update-example
   #:component-example
   #:component-prompt
   ;; search
   #:search-algorithm
   #:search
   #:search-node
   #:make-search-node
   #:search-node-state
   #:search-node-reward
   #:search-node-trace
   #:beam-search
   #:make-beam-search
   #:beam-search-beam-size
   #:beam-search-max-depth
   ;; reasoner
   #:reasoner
   #:make-reasoner
   #:solve
   #:reasoner-world-model
   #:reasoner-search-config
   #:reasoner-search-algorithm
   ;; litelm backend
   #:litelm-model
   #:make-litelm-model
   #:litelm-model-model
   #:litelm-model-max-tokens
   #:litelm-model-temperature
   #:litelm-model-retries
   ;; chain of thought
   #:cot-prompt
   #:cot-reasoner
   #:make-cot-reasoner
   #:cot-solve
   #:cot-reasoner-model
   #:cot-reasoner-few-shot
   #:cot-reasoner-instruction
   #:cot-reasoner-n-samples
   #:cot-reasoner-temperature
   ;; answer handling
   #:retrieve-answer
   #:answer-from-dataset
   #:normalize-answer
   #:answer-equal
   #:majority-vote
   #:accuracy
   ;; offline tests of this library
   #:run-self-tests))

(in-package #:llm-reasoning)

;;; ---------------------------------------------------------------------------
;;; Make sure litelm is available before we read any LITELM: symbol below.
;;;
;;; These are two separate top-level forms on purpose: a form is read before it
;;; is evaluated, so ASDF must already exist by the time the second form is
;;; READ.  (With `sbcl --script' the init file is skipped, so neither Quicklisp
;;; nor the project directory that holds litelm.asd is registered -- we have to
;;; sort both out ourselves.)
;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (unless (find-package :ql)
    (let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
      (when (probe-file setup) (load setup)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :litelm)
    ;; litelm lives in a local project directory that ~/.sbclrc normally
    ;; registers.  Under --script we look for litelm.asd ourselves.
    (labels ((litelm-asd ()
               (or (let ((env (uiop:getenv "LITELM_ASD")))
                     (and env (probe-file env)))
                   (loop for root in (list (merge-pathnames
                                            "GITHUB/loving-common-lisp/src/litelm/"
                                            (user-homedir-pathname))
                                           (merge-pathnames
                                            "quicklisp/local-projects/"
                                            (user-homedir-pathname)))
                         for asd = (merge-pathnames "litelm.asd" root)
                         when (probe-file asd) return asd)
                   (first (ignore-errors
                            (directory (merge-pathnames
                                        "GITHUB/**/litelm.asd"
                                        (user-homedir-pathname))))))))
      (let ((*print-case* :upcase)          ; see the note at the top of this file
            (asd (litelm-asd)))
        ;; ASDF wants a system name, so point its central registry at the
        ;; directory holding litelm.asd rather than passing the pathname.
        (when asd
          (pushnew (make-pathname :name nil :type nil :defaults asd)
                   asdf:*central-registry*
                   :test #'equal))
        (asdf:load-system :litelm)))))

;;; ---------------------------------------------------------------------------
;;; Protocol: language model
;;; ---------------------------------------------------------------------------

(defclass language-model () ()
  (:documentation "Base class for anything that can complete a prompt."))

(defgeneric generate (model prompt &key max-tokens temperature)
  (:documentation "Return MODEL's completion of PROMPT as a string."))

(defgeneric model-name (model)
  (:documentation "A human-readable identifier for MODEL.")
  (:method ((model language-model)) "unknown"))

;;; ---------------------------------------------------------------------------
;;; Protocol: world model and search config
;;; ---------------------------------------------------------------------------

(defclass reasoning-component ()
  ((example :initarg :example :accessor component-example :initform nil)
   (prompt  :initarg :prompt  :accessor component-prompt  :initform nil))
  (:documentation "Shared per-problem state: the example and its prompt."))

(defgeneric update-example (component example prompt)
  (:documentation "Bind EXAMPLE and PROMPT, then return COMPONENT."))

(defmethod update-example ((component reasoning-component) example prompt)
  (setf (component-example component) example
        (component-prompt component) prompt)
  component)

(defclass world-model (reasoning-component) ()
  (:documentation "State, dynamics and the terminal test for a problem."))

(defclass search-config (reasoning-component)
  ((reward-alpha :initarg :reward-alpha :accessor search-config-reward-alpha
                 :initform 0.5)
   (batch-size :initarg :batch-size :accessor search-config-batch-size
               :initform 1))
  (:documentation "Which actions to consider, and how good each one is."))

(defgeneric init-state (world-model)
  (:documentation "Return the initial state."))

(defgeneric step (world-model state action)
  (:documentation "Apply ACTION in STATE.  Returns (values next-state aux-plist),
where AUX may carry :CONFIDENCE for the default REWARD method."))

(defgeneric is-terminal (world-model state)
  (:documentation "True when STATE ends the search."))

(defgeneric get-actions (search-config state)
  (:documentation "Candidate actions in STATE."))

(defgeneric fast-reward (search-config state action)
  (:documentation "Cheap reward used to score a candidate action."))

(defgeneric reward (search-config state action &key r-useful confidence)
  (:documentation "Full reward for taking ACTION in STATE."))

(defmethod reward ((config search-config) state action &key r-useful (confidence 0.8))
  "Default reward, mirroring llm-reasoners: a weighted geometric mean of the
usefulness of the action and the confidence in the resulting state."
  (declare (ignore state action))
  (let ((alpha (search-config-reward-alpha config)))
    (* (expt (float (or r-useful 0.0)) alpha)
       (expt (float (or confidence 0.8)) (- 1 alpha)))))

;;; ---------------------------------------------------------------------------
;;; Search
;;; ---------------------------------------------------------------------------

(defstruct (search-node (:constructor make-search-node (&key state reward trace)))
  "One node of the search tree: a STATE, its accumulated REWARD and the TRACE
of actions that reached it."
  (state nil)
  (reward 0.0)
  (trace nil))

(defclass search-algorithm () ()
  (:documentation "Explores a world model; see SEARCH."))

(defgeneric search (algorithm world-model search-config)
  (:documentation "Return the best SEARCH-NODE found."))

(defun %best-nodes (nodes n)
  "The N NODES with the highest accumulated reward."
  (let ((sorted (sort (copy-list nodes) #'> :key #'search-node-reward)))
    (if (> (length sorted) n) (subseq sorted 0 n) sorted)))

(defclass beam-search (search-algorithm)
  ((beam-size :initarg :beam-size :accessor beam-search-beam-size :initform 3)
   (max-depth :initarg :max-depth :accessor beam-search-max-depth :initform 4)
   (verbose   :initarg :verbose   :accessor beam-search-verbose   :initform nil))
  (:documentation "Breadth-first beam search over a WORLD-MODEL/SEARCH-CONFIG."))

(defun make-beam-search (&key (beam-size 3) (max-depth 4) verbose)
  (make-instance 'beam-search :beam-size beam-size :max-depth max-depth
                              :verbose verbose))

(defmethod search ((algorithm beam-search) (wm world-model) (config search-config))
  (let ((beams (list (make-search-node :state (init-state wm) :reward 0.0 :trace nil))))
    (dotimes (depth (beam-search-max-depth algorithm))
      (let ((candidates '()))
        (dolist (node beams)
          (unless (is-terminal wm (search-node-state node))
            (dolist (action (get-actions config (search-node-state node)))
              (multiple-value-bind (next aux)
                  (step wm (search-node-state node) action)
                (let* ((useful (fast-reward config (search-node-state node) action))
                       (r (reward config (search-node-state node) action
                                  :r-useful useful
                                  :confidence (getf aux :confidence 0.8)))
                       (child (make-search-node
                               :state next
                               :reward (+ (search-node-reward node) r)
                               :trace (append (search-node-trace node) (list action)))))
                  (push child candidates))))))
        (when (null candidates) (return))
        (setf beams (%best-nodes candidates (beam-search-beam-size algorithm)))
        (when (some (lambda (node) (is-terminal wm (search-node-state node))) beams)
          (return))))
    (let ((best (first (%best-nodes beams 1))))
      (when (and (beam-search-verbose algorithm) best)
        (format t "~&beam-search: reward ~,4F after ~D action(s): ~S~%"
                (search-node-reward best)
                (length (search-node-trace best))
                (search-node-trace best)))
      best)))

;;; ---------------------------------------------------------------------------
;;; Reasoner: world model + search config + search algorithm
;;; ---------------------------------------------------------------------------

(defclass reasoner ()
  ((world-model :initarg :world-model :accessor reasoner-world-model)
   (search-config :initarg :search-config :accessor reasoner-search-config)
   (search-algorithm :initarg :search-algorithm :accessor reasoner-search-algorithm)))

(defun make-reasoner (&key world-model search-config
                        (search-algorithm (make-beam-search)))
  (make-instance 'reasoner :world-model world-model
                           :search-config search-config
                           :search-algorithm search-algorithm))

(defgeneric solve (reasoner example &key prompt)
  (:documentation "Run the search for EXAMPLE; returns the best SEARCH-NODE."))

(defmethod solve ((r reasoner) example &key prompt)
  (update-example (reasoner-world-model r) example prompt)
  (update-example (reasoner-search-config r) example prompt)
  (search (reasoner-search-algorithm r)
          (reasoner-world-model r)
          (reasoner-search-config r)))

;;; ---------------------------------------------------------------------------
;;; The litelm language model
;;; ---------------------------------------------------------------------------

(defclass litelm-model (language-model)
  ((model :initarg :model :accessor litelm-model-model :initform nil)
   (max-tokens :initarg :max-tokens :accessor litelm-model-max-tokens :initform 512)
   (temperature :initarg :temperature :accessor litelm-model-temperature
                :initform 0.0)
   (retries :initarg :retries :accessor litelm-model-retries :initform 3)
   (pause :initarg :pause :accessor litelm-model-pause :initform 1.0))
  (:documentation "A LANGUAGE-MODEL served by litelm.  MODEL is a
\"provider/model-name\" string; NIL means litelm's default (the local oMLX
server).  No API key is needed for oMLX."))

(defun make-litelm-model (&key model (max-tokens 512) (temperature 0.0) (retries 3))
  "Build a litelm-backed language model.  With no MODEL this is the local oMLX
model, e.g. \"omlx/Laguna-XS-2.1-6bit\"."
  (make-instance 'litelm-model :model model :max-tokens max-tokens
                               :temperature temperature :retries retries))

(defmethod model-name ((model litelm-model))
  (or (litelm-model-model model) litelm:*default-model*))

(defmethod generate ((model litelm-model) prompt &key max-tokens temperature)
  "Send PROMPT as a single user message and return the reply text.
Retries with a linear backoff, since a local server may be loading the model."
  (let ((attempt 0))
    (loop
      (incf attempt)
      (handler-case
          (let ((response (litelm:completion
                           (litelm-model-model model)
                           :messages prompt
                           :max-tokens (or max-tokens (litelm-model-max-tokens model))
                           :temperature (or temperature
                                            (litelm-model-temperature model)))))
            (return (or (litelm:response-content response) "")))
        (error (e)
          (when (>= attempt (litelm-model-retries model))
            (error "litelm completion failed after ~D attempt~:P: ~A" attempt e))
          (format *error-output* "~&litelm error (attempt ~D): ~A~%" attempt e)
          (sleep (* attempt (litelm-model-pause model))))))))

;;; ---------------------------------------------------------------------------
;;; Answers
;;; ---------------------------------------------------------------------------

(defun normalize-answer (answer)
  "Normalise ANSWER for comparison: drop $, commas and spaces, and any trailing
period.  Returns NIL for NIL or for an empty result.

ANSWER may be a string, symbol, character or number -- dataset golds are often
read as numbers, and STRING would reject those.  STRING is still used for
symbols so that *PRINT-CASE* cannot fold their names."
  (when answer
    (let* ((raw (if (typep answer '(or string symbol character))
                    (string answer)
                    (princ-to-string answer)))
           (s (string-trim '(#\Space #\Tab #\Newline #\Return) raw))
           (s (remove-if (lambda (c) (member c '(#\$ #\, #\Space))) s))
           (s (string-right-trim "." s)))
      (if (plusp (length s)) s nil))))

(defun %as-number (string)
  "STRING as a number, or NIL when it is not numeric.  *READ-EVAL* is disabled
because the input is model output."
  (when string
    (let ((value (let ((*read-eval* nil))
                   (ignore-errors (read-from-string string)))))
      (and (numberp value) value))))

(defun answer-equal (a b)
  "True when A and B denote the same answer: compared numerically when both look
like numbers, otherwise case-insensitively as strings.  NIL matches nothing."
  (let ((na (normalize-answer a))
        (nb (normalize-answer b)))
    (cond ((or (null na) (null nb)) nil)
          ((and (%as-number na) (%as-number nb))
           (= (%as-number na) (%as-number nb)))
          (t (string-equal na nb)))))

(defun %leading-number (string &key (start 0))
  "The first numeric token in STRING at or after START, or NIL.  Skips filler
such as \"$ \" and accepts a sign, commas and a decimal point."
  (let* ((length (length string))
         (i start))
    ;; skip to the first digit, then take the run of numeric characters
    (loop while (and (< i length) (not (digit-char-p (char string i))))
          do (incf i))
    (when (< i length)
      (let ((begin i)
            (j i))
        (when (and (> begin start)
                   (member (char string (1- begin)) '(#\- #\+)))
          (decf begin))
        (loop while (and (< j length)
                         (or (digit-char-p (char string j))
                             (member (char string j) '(#\, #\.))))
              do (incf j))
        (subseq string begin j)))))

(defun retrieve-answer (text)
  "Extract the final answer from a chain-of-thought completion.  Mirrors
utils.retrieve_answer in the Python CoT example: it keys on \"the answer is\"
(case-insensitively) and normalises whatever follows.  Returns NIL when the
phrase is absent, so an unparseable completion counts as wrong."
  (when (and text (stringp text))
    (let ((pos (cl:search "the answer is" text :test #'char-equal :from-end t)))
      (when pos
        (normalize-answer
         (%leading-number text :start (+ pos (length "the answer is"))))))))

(defun answer-from-dataset (gold)
  "Pull the gold answer out of a GSM8K dataset answer (everything after ####)."
  (let ((pos (cl:search "####" gold)))
    (normalize-answer (if pos (subseq gold (+ pos 4)) gold))))

(defun majority-vote (answers)
  "The most frequent non-NIL answer in ANSWERS (ties broken by first
appearance), or NIL when every answer is NIL."
  (let ((counts '())
        (best nil)
        (best-count 0))
    (dolist (answer answers)
      (when answer
        (let ((cell (assoc answer counts :test #'answer-equal)))
          (if cell
              (incf (cdr cell))
              (push (cons answer 1) counts)))))
    (dolist (cell (reverse counts))
      (when (> (cdr cell) best-count)
        (setf best (car cell)
              best-count (cdr cell))))
    best))

(defun accuracy (predictions golds)
  "Fraction of PREDICTIONS that match the corresponding gold answers."
  (assert (= (length predictions) (length golds)) ()
          "ACCURACY needs the same number of predictions (~D) and golds (~D)"
          (length predictions) (length golds))
  (if (null predictions)
      0.0
      (/ (count-if #'identity (mapcar #'answer-equal predictions golds))
         (float (length predictions)))))

;;; ---------------------------------------------------------------------------
;;; Chain of thought
;;; ---------------------------------------------------------------------------

(defun cot-prompt (few-shot question &key instruction)
  "A chain-of-thought prompt: FEW-SHOT is a list of \"Q: ... A: ...\" strings,
each ending in a blank line; QUESTION is the problem to answer.  INSTRUCTION,
when given, is prepended to the whole prompt (the Python example uses this to
demand a \"So the answer is\" ending)."
  (concatenate 'string
               (or instruction "")
               (apply #'concatenate 'string few-shot)
               "Q: " question (string #\Newline) "A:"))

(defclass cot-reasoner ()
  ((model :initarg :model :accessor cot-reasoner-model)
   (few-shot :initarg :few-shot :accessor cot-reasoner-few-shot :initform nil)
   (instruction :initarg :instruction :accessor cot-reasoner-instruction
                :initform nil)
   (n-samples :initarg :n-samples :accessor cot-reasoner-n-samples :initform 1)
   (temperature :initarg :temperature :accessor cot-reasoner-temperature
                :initform 0.0))
  (:documentation "Chain-of-thought, optionally with self-consistency."))

(defun make-cot-reasoner (&key model few-shot instruction (n-samples 1)
                            (temperature 0.0))
  "Build a chain-of-thought reasoner.  With N-SAMPLES greater than 1 the chains
are sampled (so TEMPERATURE should be positive) and the answers are combined by
majority vote -- self-consistency."
  (make-instance 'cot-reasoner :model model :few-shot few-shot
                               :instruction instruction
                               :n-samples n-samples :temperature temperature))

(defgeneric cot-solve (reasoner question)
  (:documentation "Answer QUESTION.  Returns (values answer completions)."))

(defmethod cot-solve ((r cot-reasoner) question)
  (let* ((prompt (cot-prompt (cot-reasoner-few-shot r) question
                             :instruction (cot-reasoner-instruction r)))
         (n (max 1 (cot-reasoner-n-samples r)))
         (completions '())
         (answers '()))
    (dotimes (i n)
      (let* ((text (generate (cot-reasoner-model r) prompt
                             :temperature (cot-reasoner-temperature r)))
             (answer (retrieve-answer text)))
        (push text completions)
        (push answer answers)))
    (setf completions (nreverse completions)
          answers (nreverse answers))
    (values (if (= n 1)
                (first answers)
                (majority-vote answers))
            completions)))

;;; ---------------------------------------------------------------------------
;;; A toy world model, so the search framework can be checked with no model
;;; ---------------------------------------------------------------------------

(defclass sum-to-ten-world-model (world-model)
  ((target :initarg :target :accessor sum-target :initform 10))
  (:documentation "Add 1, 2 or 3 until TARGET is reached.  Used by
RUN-SELF-TESTS: it needs no language model."))

(defmethod init-state ((wm sum-to-ten-world-model)) 0)

(defmethod is-terminal ((wm sum-to-ten-world-model) state)
  (>= state (sum-target wm)))

(defmethod step ((wm sum-to-ten-world-model) state action)
  (let ((next (+ state action)))
    (values next (list :confidence (if (= next (sum-target wm)) 1.0 0.5)))))

(defclass sum-to-ten-config (search-config)
  ((target :initarg :target :accessor sum-target :initform 10)))

(defmethod get-actions ((config sum-to-ten-config) state)
  (if (>= state (sum-target config)) '() '(1 2 3)))

(defmethod fast-reward ((config sum-to-ten-config) state action)
  (declare (ignore state action))
  1.0)

(defmethod reward ((config sum-to-ten-config) state action
                   &key r-useful confidence)
  "1.0 for landing exactly on the target, a small monotone nudge towards larger
sums while still below it, and 0.0 for overshooting -- which keeps the expected
beam deterministic and makes hitting the target strictly best."
  (declare (ignore r-useful confidence))
  (let ((next (+ state action))
        (target (sum-target config)))
    (cond ((= next target) 1.0)
          ((< next target) (* 0.9 (/ next (float target))))
          (t 0.0))))

;;; ---------------------------------------------------------------------------
;;; Offline self-tests
;;; ---------------------------------------------------------------------------

(defun %check (name thunk)
  (handler-case
      (if (funcall thunk)
          (progn (format t "~&  ok    ~A~%" name) t)
          (progn (format t "~&  FAIL  ~A~%" name) nil))
    (error (e)
      (format t "~&  ERROR ~A: ~A~%" name e)
      nil)))

(defun run-self-tests ()
  "Check the answer handling and the search framework.  No model is contacted.
Returns T when everything passes, otherwise signals an error."
  (let ((passed 0)
        (total 0))
    (labels ((check (name thunk)
               (incf total)
               (when (%check name thunk) (incf passed))))
      (format t "~&llm-reasoning self-tests~%")

      ;; --- answer handling ---
      (check "normalize-answer strips $ , and trailing period"
             (lambda () (equal (normalize-answer "$1,000.") "1000")))
      (check "normalize-answer accepts a number"
             (lambda () (equal (normalize-answer 18) "18")))
      (check "answer-equal compares a numeric gold to a string answer"
             (lambda () (and (answer-equal "18" 18) (answer-equal 18.0 "18"))))
      (check "accuracy accepts numeric golds"
             (lambda () (= (accuracy '("18" "3") '(18 4)) 0.5)))
      (check "answer-equal ignores a trailing period"
             (lambda () (answer-equal "18." "18")))
      (check "answer-equal compares numerically"
             (lambda () (answer-equal "18.0" "18")))
      (check "answer-equal rejects different answers"
             (lambda () (not (answer-equal "18" "3"))))
      (check "answer-equal rejects NIL"
             (lambda () (not (answer-equal nil "18"))))
      (check "retrieve-answer finds the value after 'the answer is'"
             (lambda () (equal (retrieve-answer
                                "She sold 48 + 24 = 72 clips. The answer is 72.")
                               "72")))
      (check "retrieve-answer is case-insensitive"
             (lambda () (equal (retrieve-answer "The Answer Is 5.") "5")))
      (check "retrieve-answer returns NIL without the phrase"
             (lambda () (null (retrieve-answer "just some text"))))
      (check "retrieve-answer handles a trailing dollar amount"
             (lambda () (equal (retrieve-answer "The answer is $18.") "18")))
      (check "answer-from-dataset takes what follows ####"
             (lambda () (equal (answer-from-dataset
                                "reasoning here\n#### 42") "42")))
      (check "majority-vote picks the most frequent answer"
             (lambda () (equal (majority-vote '("18" nil "18" "3")) "18")))
      (check "majority-vote returns NIL for all-NIL"
             (lambda () (null (majority-vote '(nil nil)))))
      (check "accuracy counts matches"
             (lambda () (= (accuracy '("18" "3") '("18" "4")) 0.5)))

      ;; --- search framework ---
      (check "beam search reaches the toy target"
             (lambda ()
               (let* ((wm (make-instance 'sum-to-ten-world-model :target 10))
                      (config (make-instance 'sum-to-ten-config :target 10))
                      (algo (make-beam-search :beam-size 5 :max-depth 10))
                      (best (search algo wm config)))
                 (and best (= (search-node-state best) 10)))))
      (check "beam search reports a reward for the solution"
             (lambda ()
               (let* ((wm (make-instance 'sum-to-ten-world-model :target 10))
                      (config (make-instance 'sum-to-ten-config :target 10))
                      (algo (make-beam-search :beam-size 5 :max-depth 10))
                      (best (search algo wm config)))
                 (and best (>= (search-node-reward best) 1.0)))))
      (check "beam search records the actions it took"
             (lambda ()
               (let* ((wm (make-instance 'sum-to-ten-world-model :target 10))
                      (config (make-instance 'sum-to-ten-config :target 10))
                      (algo (make-beam-search :beam-size 5 :max-depth 10))
                      (best (search algo wm config)))
                 (and best (plusp (length (search-node-trace best)))))))
      (check "a REASONER drives the same search"
             (lambda ()
               (let* ((wm (make-instance 'sum-to-ten-world-model :target 10))
                      (config (make-instance 'sum-to-ten-config :target 10))
                      (r (make-reasoner :world-model wm :search-config config
                                        :search-algorithm
                                        (make-beam-search :beam-size 5
                                                          :max-depth 10))))
                 (= (search-node-state (solve r "sum to 10" :prompt nil)) 10)))))

    (format t "~&~D/~D checks passed~%" passed total)
    (if (= passed total)
        t
        (error "~D llm-reasoning self-test~:P failed" (- total passed)))))
