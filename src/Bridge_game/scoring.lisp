;;;; scoring.lisp — Rubber Bridge Scoring
;;;;
;;;; Implements rubber bridge scoring:
;;;; - Below the line: trick score (only tricks bid and made)
;;;; - Above the line: overtricks, slam/insult bonuses, penalties
;;;; - Game: below-the-line total reaches 100+
;;;; - Rubber: first side to win 2 games
;;;; - Vulnerability: winning a game makes that side vulnerable
;;;; - Rubber bonus: 700 (2-0) or 500 (2-1)

(in-package #:bridge)

;;; ============================================================
;;; RANDOM STATE — seed from OS for different hands every run
;;; ============================================================

(setf *random-state* (make-random-state t))

;;; ============================================================
;;; TRICK VALUES
;;; ============================================================

(defun trick-value (suit &optional (doubled 0))
  "Base value per trick for a given strain.
   Clubs/Diamonds = 20, Hearts/Spades = 30, NT = 30 (40 for first)."
  (let ((base (cond
                ((<= suit +diamonds+) 20)  ; Minors
                (t 30))))                   ; Majors and NT
    (case doubled
      (1 (* base 2))
      (2 (* base 4))
      (t base))))

(defun contract-trick-score (level suit &optional (doubled 0))
  "Calculate the below-the-line trick score for a contract.
   Only counts tricks actually bid (not overtricks)."
  (let ((base-per-trick (trick-value suit doubled)))
    (+ (if (and (= suit +notrump+) (= doubled 0))
           10  ; Extra 10 for first NT trick
           (if (and (= suit +notrump+) (= doubled 1))
               20
               (if (and (= suit +notrump+) (= doubled 2))
                   40
                   0)))
       (* base-per-trick level))))

(defun game-contract-p (level suit &optional (doubled 0))
  "Is this contract worth game (trick score >= 100)?"
  (>= (contract-trick-score level suit doubled) 100))

(defun slam-p (level)
  "Is this a slam (6-level) or grand slam (7-level)?"
  (>= level 6))

(defun grand-slam-p (level)
  (= level 7))

;;; ============================================================
;;; RUBBER BRIDGE STATE
;;; ============================================================

(defstruct rubber-state
  "State of an ongoing rubber bridge match."
  ;; Below the line: trick points toward game
  (ns-below      0)    ; N-S points below the line (current game)
  (ew-below      0)    ; E-W points below the line (current game)
  ;; Above the line: bonuses and penalties
  (ns-above      0)    ; N-S points above the line (cumulative)
  (ew-above      0)    ; E-W points above the line (cumulative)
  ;; Games won
  (ns-games      0)    ; N-S games won in this rubber
  (ew-games      0)    ; E-W games won in this rubber
  ;; Derived vulnerability (updated after each game)
  (ns-vulnerable nil)
  (ew-vulnerable nil)
  ;; Dealer rotates each deal
  (current-dealer +north+)
  ;; History of deals played
  (deals-played   0))

(defun rubber-vulnerability (rs)
  "Return vulnerability keyword based on rubber state."
  (cond
    ((and (rubber-state-ns-vulnerable rs)
          (rubber-state-ew-vulnerable rs)) :both)
    ((rubber-state-ns-vulnerable rs) :ns)
    ((rubber-state-ew-vulnerable rs) :ew)
    (t :none)))

(defun rubber-complete-p (rs)
  "Is the rubber complete? (one side has won 2 games)"
  (or (>= (rubber-state-ns-games rs) 2)
      (>= (rubber-state-ew-games rs) 2)))

;;; ============================================================
;;; RUBBER BRIDGE SCORE CALCULATION
;;; ============================================================

(defun score-rubber-deal (level suit tricks-won declarer doubled
                          rubber-state)
  "Score a single deal in rubber bridge.
   Updates the rubber-state in place.
   Returns the below-the-line trick score for display.
   
   In rubber bridge:
   - Below the line: trick score for tricks bid and made
   - Above the line: overtricks, slam bonuses, insult bonuses
   - Penalties for going down go above the line for defenders"
  (let* ((tricks-needed (+ level 6))
         (overtricks (- tricks-won tricks-needed))
         (made (>= tricks-won tricks-needed))
         (ns-side (evenp declarer))
         (vul (if ns-side
                  (rubber-state-ns-vulnerable rubber-state)
                  (rubber-state-ew-vulnerable rubber-state))))

    (if made
        ;; === CONTRACT MADE ===
        (let ((below-score (contract-trick-score level suit doubled))
              (above-score 0))

          ;; Overtrick bonus (above the line)
          (when (> overtricks 0)
            (incf above-score
                  (case doubled
                    (0 (* overtricks (trick-value suit)))
                    (1 (* overtricks (if vul 200 100)))
                    (2 (* overtricks (if vul 400 200))))))

          ;; Insult bonus for making doubled/redoubled (above the line)
          (case doubled
            (1 (incf above-score 50))
            (2 (incf above-score 100)))

          ;; Slam bonus (above the line)
          (when (slam-p level)
            (incf above-score (if vul 750 500)))
          (when (grand-slam-p level)
            (incf above-score (if vul 1500 1000)))

          ;; Record scores
          (if ns-side
              (progn
                (incf (rubber-state-ns-below rubber-state) below-score)
                (incf (rubber-state-ns-above rubber-state) above-score))
              (progn
                (incf (rubber-state-ew-below rubber-state) below-score)
                (incf (rubber-state-ew-above rubber-state) above-score)))

          ;; Check if this side made game (below >= 100)
          (when (if ns-side
                    (>= (rubber-state-ns-below rubber-state) 100)
                    (>= (rubber-state-ew-below rubber-state) 100))
            ;; Game won!
            (if ns-side
                (progn
                  (incf (rubber-state-ns-games rubber-state))
                  (setf (rubber-state-ns-vulnerable rubber-state) t))
                (progn
                  (incf (rubber-state-ew-games rubber-state))
                  (setf (rubber-state-ew-vulnerable rubber-state) t)))
            ;; Start new game — reset BOTH sides' below-the-line scores
            (setf (rubber-state-ns-below rubber-state) 0
                  (rubber-state-ew-below rubber-state) 0))

          below-score)

        ;; === CONTRACT DEFEATED ===
        (let* ((down (abs overtricks))
               (penalty
                 (case doubled
                   ;; Undoubled penalties
                   (0 (* down (if vul 100 50)))
                   ;; Doubled penalties
                   (1 (if vul
                          (+ 200 (* (max 0 (1- down)) 300))
                          (cond
                            ((= down 1) 100)
                            ((= down 2) 300)
                            ((= down 3) 500)
                            (t (+ 500 (* (- down 3) 300))))))
                   ;; Redoubled
                   (2 (if vul
                          (* 2 (+ 200 (* (max 0 (1- down)) 300)))
                          (* 2 (cond
                                 ((= down 1) 100)
                                 ((= down 2) 300)
                                 ((= down 3) 500)
                                 (t (+ 500 (* (- down 3) 300))))))))))

          ;; Penalties go above the line for the defenders
          (if ns-side
              (incf (rubber-state-ew-above rubber-state) penalty)
              (incf (rubber-state-ns-above rubber-state) penalty))

          (- penalty)))))

;;; ============================================================
;;; RUBBER BONUS
;;; ============================================================

(defun rubber-bonus (rs)
  "Calculate the rubber bonus for the winning side.
   700 for winning 2-0, 500 for winning 2-1.
   Returns (values bonus winning-side)."
  (cond
    ((>= (rubber-state-ns-games rs) 2)
     (values (if (= (rubber-state-ew-games rs) 0) 700 500) :ns))
    ((>= (rubber-state-ew-games rs) 2)
     (values (if (= (rubber-state-ns-games rs) 0) 700 500) :ew))
    (t (values 0 nil))))

(defun rubber-total-scores (rs)
  "Calculate final rubber totals for each side.
   Returns (values ns-total ew-total)."
  (multiple-value-bind (bonus winner) (rubber-bonus rs)
    (let ((ns-total (+ (rubber-state-ns-above rs)))
          (ew-total (+ (rubber-state-ew-above rs))))
      (case winner
        (:ns (incf ns-total bonus))
        (:ew (incf ew-total bonus)))
      (values ns-total ew-total))))

;;; ============================================================
;;; VULNERABILITY
;;; ============================================================

(defun vulnerable-p (declarer vulnerability)
  "Is the declarer's side vulnerable?
   vulnerability: :none, :ns, :ew, :both"
  (let ((side (mod declarer 2)))  ; 0=NS, 1=EW
    (case vulnerability
      (:none nil)
      (:ns (= side 0))
      (:ew (= side 1))
      (:both t)
      (t nil))))

;;; ============================================================
;;; DISPLAY
;;; ============================================================

(defun display-rubber-score (rs &optional (stream t))
  "Display the current rubber bridge scorecard."
  (format stream "~%╔════════════════════╦════════════════════╗~%")
  (format stream   "║   RUBBER BRIDGE SCORECARD              ║~%")
  (format stream   "╠════════════════════╬════════════════════╣~%")
  (format stream   "║      N-S           ║      E-W           ║~%")
  (format stream   "╠════════════════════╬════════════════════╣~%")
  (format stream   "║  Above: ~5D       ║  Above: ~5D       ║~%"
          (rubber-state-ns-above rs) (rubber-state-ew-above rs))
  (format stream   "╠────────────────────╬────────────────────╣~%")
  (format stream   "║  Below: ~5D       ║  Below: ~5D       ║~%"
          (rubber-state-ns-below rs) (rubber-state-ew-below rs))
  (format stream   "║  Games: ~D         ║  Games: ~D         ║~%"
          (rubber-state-ns-games rs) (rubber-state-ew-games rs))
  (format stream   "║  Vul:   ~3A       ║  Vul:   ~3A       ║~%"
          (if (rubber-state-ns-vulnerable rs) "YES" "No ")
          (if (rubber-state-ew-vulnerable rs) "YES" "No "))
  (format stream   "╚════════════════════╩════════════════════╝~%"))

(defun display-deal-result (level suit tricks-won declarer doubled
                            deal-score &optional (stream t))
  "Display the result of a single deal."
  (let* ((tricks-needed (+ level 6))
         (result (- tricks-won tricks-needed)))
    (format stream "~%  ── Deal Result ──~%")
    (format stream "  Contract: ~D~A~A by ~A~%"
            level (aref +suit-symbols+ suit)
            (case doubled (1 " Doubled") (2 " Redoubled") (t ""))
            (aref +player-names+ declarer))
    (format stream "  Tricks needed: ~D  Tricks won: ~D~%"
            tricks-needed tricks-won)
    (if (>= result 0)
        (format stream "  Made ~A~A  (below the line: ~D)~%"
                (if (= result 0) "exactly" (format nil "+~D" result))
                (if (>= result 0) "" "")
                (if (> deal-score 0) deal-score 0))
        (format stream "  Down ~D  (penalty: ~D to defenders)~%"
                (abs result) (abs deal-score)))
    (format stream "  ─────────────────────~%")))

(defun display-rubber-final (rs &optional (stream t))
  "Display final rubber results."
  (multiple-value-bind (bonus winner) (rubber-bonus rs)
    (multiple-value-bind (ns-total ew-total) (rubber-total-scores rs)
      (format stream "~%╔═════════════════════════════════════════╗~%")
      (format stream   "║         RUBBER COMPLETE!                ║~%")
      (format stream   "╠═════════════════════════════════════════╣~%")
      (format stream   "║  ~A wins the rubber (~D-~D)              ║~%"
              (case winner (:ns "N-S") (:ew "E-W"))
              (if (eq winner :ns) (rubber-state-ns-games rs) (rubber-state-ew-games rs))
              (if (eq winner :ns) (rubber-state-ew-games rs) (rubber-state-ns-games rs)))
      (format stream   "║  Rubber bonus: ~D                       ║~%"
              bonus)
      (format stream   "╠─────────────────────────────────────────╣~%")
      (format stream   "║  N-S total: ~5D                        ║~%"  ns-total)
      (format stream   "║  E-W total: ~5D                        ║~%"  ew-total)
      (format stream   "║  Net: ~A +~D~A║~%"
              (if (> ns-total ew-total) "N-S" "E-W")
              (abs (- ns-total ew-total))
              (make-string (max 1 (- 30 (length (format nil "~D" (abs (- ns-total ew-total))))))
                           :initial-element #\Space))
      (format stream   "╚═════════════════════════════════════════╝~%"))))

(defun display-score (score declarer &optional (stream t))
  "Display the deal score summary."
  (let ((side (if (or (= declarer +north+) (= declarer +south+)) "N-S" "E-W")))
    (if (>= score 0)
        (format stream "  ~A scored ~D points.~%" side score)
        (format stream "  ~A's opponents scored ~D points.~%"
                side (abs score)))))
