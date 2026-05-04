;;;; bidding.lisp — Bid Data Structures and Bidding Logic
;;;;
;;;; Implements:
;;;; - Bid representation (level + strain)
;;;; - Bid ordering and comparison
;;;; - Standard American bidding system (simplified)
;;;; - AI bidding logic with hand evaluation

(in-package #:bridge)

;;; ============================================================
;;; BID STRUCTURE
;;; ============================================================

;; Bid types:
;;   :suit  — a normal bid (level 1-7, suit 0-4)
;;   :pass  — pass
;;   :double — double
;;   :redouble — redouble

(defstruct (bid (:constructor %make-bid)
                (:print-function print-bid))
  "A bid in the auction."
  (type  :suit :type keyword)      ; :suit, :pass, :double, :redouble
  (level 1     :type fixnum)       ; 1-7
  (suit  0     :type fixnum))      ; 0=♣, 1=♦, 2=♥, 3=♠, 4=NT

(defun make-bid (level suit)
  "Create a normal suit bid."
  (%make-bid :type :suit :level level :suit suit))

(defun make-pass ()
  (%make-bid :type :pass :level 0 :suit 0))

(defun make-double-bid ()
  (%make-bid :type :double :level 0 :suit 0))

(defun make-redouble-bid ()
  (%make-bid :type :redouble :level 0 :suit 0))

(defun print-bid (bid stream depth)
  (declare (ignore depth))
  (case (bid-type bid)
    (:pass     (format stream "Pass"))
    (:double   (format stream "Dbl"))
    (:redouble (format stream "Rdbl"))
    (:suit     (format stream "~D~A"
                       (bid-level bid)
                       (aref +suit-symbols+ (bid-suit bid))))))

(defun bid-name (bid)
  "Full display name of a bid."
  (case (bid-type bid)
    (:pass     "Pass")
    (:double   "Double")
    (:redouble "Redouble")
    (:suit     (format nil "~D ~A"
                       (bid-level bid)
                       (aref +suit-names+ (bid-suit bid))))))

(defun bid-pass-p (bid)
  (eq (bid-type bid) :pass))

(defun bid-double-p (bid)
  (eq (bid-type bid) :double))

(defun bid-redouble-p (bid)
  (eq (bid-type bid) :redouble))

(defun bid-suit-p (bid)
  (eq (bid-type bid) :suit))

;;; ============================================================
;;; BID COMPARISON
;;; ============================================================

(defun bid-index (bid)
  "Numeric index for comparing bids. 1♣=0, 1♦=1, ..., 7NT=34."
  (when (bid-suit-p bid)
    (+ (* (1- (bid-level bid)) 5)
       (bid-suit bid))))

(defun bid> (b1 b2)
  "Is bid b1 higher than bid b2? (both must be suit bids)"
  (and (bid-suit-p b1)
       (bid-suit-p b2)
       (> (bid-index b1) (bid-index b2))))

(defun bid>= (b1 b2)
  (or (and (bid-suit-p b1) (bid-suit-p b2)
           (= (bid-index b1) (bid-index b2)))
      (bid> b1 b2)))

(defun minimum-next-bid (current-bid)
  "Return the minimum legal suit bid after current-bid."
  (if (null current-bid)
      (make-bid 1 +clubs+)
      (let ((idx (bid-index current-bid)))
        (if (< idx 34)
            (let ((next (1+ idx)))
              (make-bid (1+ (floor next 5))
                        (mod next 5)))
            nil)))) ; No higher bid possible

;;; ============================================================
;;; BIDDING HISTORY ANALYSIS
;;; ============================================================

(defun last-suit-bid (bid-history)
  "Find the most recent suit bid in the auction."
  (find-if #'bid-suit-p bid-history))

(defun bidding-complete-p (bid-history)
  "The bidding ends after 3 consecutive passes (following at least one suit bid),
   or 4 passes if no one opened."
  (let ((len (length bid-history)))
    (cond
      ;; Less than 4 bids: not complete
      ((< len 4) nil)
      ;; 4 passes = passed out
      ((and (= len 4)
            (every #'bid-pass-p bid-history))
       t)
      ;; 3 passes after a suit bid
      ((and (>= len 4)
            (bid-pass-p (nth 0 bid-history))
            (bid-pass-p (nth 1 bid-history))
            (bid-pass-p (nth 2 bid-history))
            (not (every #'bid-pass-p bid-history)))
       t)
      (t nil))))

(defun passed-out-p (bid-history)
  "Was the deal passed out (no one opened)?"
  (and (= (length bid-history) 4)
       (every #'bid-pass-p bid-history)))

(defun extract-contract (bid-history dealer)
  "Extract the final contract from the bidding.
   Returns (values contract-bid declarer doubled-state).
   doubled-state: 0=undoubled, 1=doubled, 2=redoubled."
  (let ((final-bid nil)
        (final-bidder nil)
        (doubled 0))
    ;; Walk backward through bid history to find the last suit bid
    (loop for i from 0 below (length bid-history)
          for bid = (nth i bid-history)
          do (cond
               ((bid-suit-p bid)
                (when (null final-bid)
                  (setf final-bid bid
                        final-bidder (mod (+ dealer (- (length bid-history) 1 i)) 4))))
               ((bid-double-p bid)
                (when (and final-bid (= doubled 0))
                  (setf doubled 1)))
               ((bid-redouble-p bid)
                (when (= doubled 1)
                  (setf doubled 2)))))
    ;; Find the actual declarer — the first player on the declaring side
    ;; who bid the contract strain (walking chronologically, oldest first)
    (when final-bid
      (let* ((declaring-side (mod final-bidder 2))
             (contract-suit (bid-suit final-bid))
             (declarer final-bidder))
        ;; Walk chronologically (oldest bid first = highest index in our
        ;; most-recent-first list) and stop at the first match
        (loop for i from (1- (length bid-history)) downto 0
              for bid = (nth i bid-history)
              for bidder = (mod (+ dealer (- (length bid-history) 1 i)) 4)
              do (when (and (bid-suit-p bid)
                            (= (bid-suit bid) contract-suit)
                            (= (mod bidder 2) declaring-side))
                   (setf declarer bidder)
                   (return)))  ; Stop at the FIRST one
        (values final-bid declarer doubled)))))

;;; ============================================================
;;; AI BIDDING — STANDARD AMERICAN (SIMPLIFIED)
;;; ============================================================

(defun hand-suit-honor-count (hand suit)
  "Count honors (A, K, Q, J, 10) in a specific suit."
  (count-if (lambda (c) (and (= (card-suit c) suit)
                              (>= (card-rank c) 10)))
            hand))

(defun ai-opening-bid (hand)
  "Determine AI's opening bid using Standard American conventions.
   Rules from BRIDGE_BIDDING_DECISIONS.md."
  (let ((hcp (hand-hcp hand))
        (balanced (hand-balanced-p hand)))
    (cond
      ;; 2♣ — Very strong hand (22+ HCP), artificial forcing
      ((>= hcp 22)
       (make-bid 2 +clubs+))

      ;; 2NT — Balanced, 20-21 HCP
      ((and balanced (>= hcp 20) (<= hcp 21))
       (make-bid 2 +notrump+))

      ;; 1NT — Balanced, 15-17 HCP (no singleton, good doubleton OK)
      ((and balanced (>= hcp 15) (<= hcp 17))
       (make-bid 1 +notrump+))

      ;; 1 of a suit — 13-21 HCP
      ((and (>= hcp 13) (<= hcp 21))
       (cond
         ;; Both 5-card majors: bid the higher-ranking (spades)
         ((and (>= (hand-suit-length hand +spades+) 5)
               (>= (hand-suit-length hand +hearts+) 5))
          (make-bid 1 +spades+))
         ;; 5+ card major — bid it
         ((>= (hand-suit-length hand +spades+) 5)
          (make-bid 1 +spades+))
         ((>= (hand-suit-length hand +hearts+) 5)
          (make-bid 1 +hearts+))
         ;; Minor: 3+ cards, longer minor wins
         ((> (hand-suit-length hand +diamonds+)
             (hand-suit-length hand +clubs+))
          (make-bid 1 +diamonds+))
         ;; Equal minors or longer clubs
         (t (make-bid 1 +clubs+))))

      ;; Preemptive 3-level — good 7-card suit with 2+ honors, sub-opening
      ((and (< hcp 13)
            (let ((seven-card-suit
                    (loop for suit in +suits+
                          when (and (>= (hand-suit-length hand suit) 7)
                                    (>= (hand-suit-honor-count hand suit) 2))
                          return suit)))
              seven-card-suit))
       (let ((seven-card-suit
               (loop for suit in +suits+
                     when (and (>= (hand-suit-length hand suit) 7)
                               (>= (hand-suit-honor-count hand suit) 2))
                     return suit)))
         (make-bid 3 seven-card-suit)))

      ;; Weak twos — Less than opening hand, 6-card suit (not clubs)
      ((and (< hcp 13) (>= hcp 5))
       (let ((six-card-suit
               (loop for suit in (list +spades+ +hearts+ +diamonds+)
                     when (>= (hand-suit-length hand suit) 6)
                     return suit)))
         (when six-card-suit
           (make-bid 2 six-card-suit))))

      ;; Pass
      (t nil))))

(defun ai-responding-bid (hand partner-bid bid-history my-position dealer)
  "Determine the responder's bid after partner has opened.
   Rules from BRIDGE_BIDDING_DECISIONS.md."
  (declare (ignore bid-history my-position dealer))
  (let ((hcp (hand-hcp hand))
        (p-suit (bid-suit partner-bid))
        (p-level (bid-level partner-bid)))

    (cond
      ;; ── Responding to 1NT opening (partner has 15-17) ──
      ((and (= p-level 1) (= p-suit +notrump+))
       (cond
         ;; 0-7 pts with 5+ card suit: sign off at 2-level
         ((and (<= hcp 7)
               (or (>= (hand-suit-length hand +diamonds+) 5)
                   (>= (hand-suit-length hand +hearts+) 5)
                   (>= (hand-suit-length hand +spades+) 5)))
          (cond ((>= (hand-suit-length hand +spades+) 5)
                 (make-bid 2 +spades+))
                ((>= (hand-suit-length hand +hearts+) 5)
                 (make-bid 2 +hearts+))
                (t (make-bid 2 +diamonds+))))
         ;; 0-7: Pass
         ((<= hcp 7) nil)
         ;; Stayman: 8+ pts with a 4-card major, bid 2♣
         ((and (>= hcp 8)
               (or (>= (hand-suit-length hand +hearts+) 4)
                   (>= (hand-suit-length hand +spades+) 4)))
          (make-bid 2 +clubs+))
         ;; 8-9: 2NT (invitational), no 8+ card suit fit
         ((<= hcp 9) (make-bid 2 +notrump+))
         ;; 10-15 with 6+ card major: sign off at game
         ((and (<= hcp 15)
               (>= (hand-suit-length hand +spades+) 6))
          (make-bid 4 +spades+))
         ((and (<= hcp 15)
               (>= (hand-suit-length hand +hearts+) 6))
          (make-bid 4 +hearts+))
         ;; 10-15 with 5-card major: bid 3♥/3♠ (invitational)
         ((and (<= hcp 15)
               (>= (hand-suit-length hand +spades+) 5))
          (make-bid 3 +spades+))
         ((and (<= hcp 15)
               (>= (hand-suit-length hand +hearts+) 5))
          (make-bid 3 +hearts+))
         ;; 10-15: 3NT
         ((<= hcp 15) (make-bid 3 +notrump+))
         ;; 16+: Slam exploration (4NT quantitative)
         (t (make-bid 4 +notrump+))))

      ;; ── Responding to 2NT opening (partner has 20-21) ──
      ((and (= p-level 2) (= p-suit +notrump+))
       (cond
         ;; 0-4 with no 5+ card suit: pass
         ((and (<= hcp 4)
               (< (hand-suit-length hand +spades+) 5)
               (< (hand-suit-length hand +hearts+) 5)
               (< (hand-suit-length hand +diamonds+) 5)
               (< (hand-suit-length hand +clubs+) 5))
          nil)
         ;; Stayman: 4+ pts with 4-card major, bid 3♣
         ((and (>= hcp 4)
               (or (>= (hand-suit-length hand +hearts+) 4)
                   (>= (hand-suit-length hand +spades+) 4)))
          (make-bid 3 +clubs+))
         ;; With values, bid 3NT
         ((>= hcp 5) (make-bid 3 +notrump+))
         ;; Otherwise pass
         (t nil)))

      ;; ── Responding to a major suit opening (1♥ or 1♠) ──
      ((and (= p-level 1) (>= p-suit +hearts+))
       (cond
         ;; 0-5 pts: Pass
         ((<= hcp 5) nil)
         ;; 6-10 pts: Raise to 2-Level
         ((<= hcp 10)
          (if (>= (hand-suit-length hand p-suit) 3)
              (make-bid 2 p-suit)
              ;; No fit: bid 1NT or a new suit
              (cond ((and (= p-suit +hearts+)
                          (>= (hand-suit-length hand +spades+) 4))
                     (make-bid 1 +spades+))
                    (t (make-bid 1 +notrump+)))))
         ;; 11-12 pts: Raise to 3-Level
         ((<= hcp 12)
          (if (>= (hand-suit-length hand p-suit) 3)
              (make-bid 3 p-suit)
              (let ((longest (hand-longest-suit hand)))
                (if (> longest p-suit)
                    (make-bid 2 longest)
                    (make-bid 2 +notrump+)))))
         ;; 13+ pts: Bid to Game
         (t
          (if (>= (hand-suit-length hand p-suit) 3)
              (make-bid 4 p-suit)
              (make-bid 3 +notrump+)))))

      ;; ── Responding to a minor suit opening (1♣ or 1♦) ──
      ;; Priorities: 1. Major suit → 2. Notrump → 3. Minor support
      ((and (= p-level 1) (<= p-suit +diamonds+))
       (cond
         ;; Less than 6 pts: pass
         ((< hcp 6) nil)
         ;; Priority 1: Bid a Major (1♥/1♠) with 6+ pts and 4+ cards
         ;; Tie-breakers: cheapest 4-card suit, or higher-ranking 5-card suit
         ((and (>= hcp 6)
               (or (>= (hand-suit-length hand +hearts+) 4)
                   (>= (hand-suit-length hand +spades+) 4)))
          (let ((h-len (hand-suit-length hand +hearts+))
                (s-len (hand-suit-length hand +spades+)))
            (cond
              ;; Both 5+ card majors: bid the higher-ranking
              ((and (>= h-len 5) (>= s-len 5))
               (make-bid 1 +spades+))
              ;; Both 4-card majors: bid the cheapest (hearts)
              ((and (>= h-len 4) (>= s-len 4))
               (make-bid 1 +hearts+))
              ;; Single 4+ card major
              ((>= h-len 4) (make-bid 1 +hearts+))
              (t (make-bid 1 +spades+)))))
         ;; Priority 2: Bid Notrump
         ;; 6-10: 1NT
         ((<= hcp 10) (make-bid 1 +notrump+))
         ;; 11-12: 2NT
         ((<= hcp 12)
          (if (>= (hand-suit-length hand p-suit) 4)
              ;; Priority 3: Raise the minor with 4+ support
              (make-bid 3 p-suit)  ; 11-12 raise to 3-level
              (make-bid 2 +notrump+)))
         ;; 13-15: 3NT or raise minor to game with 4+ support
         ((<= hcp 15)
          (if (>= (hand-suit-length hand p-suit) 4)
              (make-bid 5 p-suit)  ; Game in a minor = 5-level
              (make-bid 3 +notrump+)))
         ;; 16+: Slam exploration
         (t (make-bid 4 +notrump+))))

      ;; ── Responding to 2♣ (strong, artificial, forcing) ──
      ((and (= p-level 2) (= p-suit +clubs+))
       (cond
         ;; < 8 pts: 2♦ (artificial waiting)
         ((< hcp 8) (make-bid 2 +diamonds+))
         ;; 8+ pts, unbalanced with good 5+ card suit
         ((and (not (hand-balanced-p hand))
               (>= (hand-suit-length hand +spades+) 5))
          (make-bid 2 +spades+))
         ((and (not (hand-balanced-p hand))
               (>= (hand-suit-length hand +hearts+) 5))
          (make-bid 2 +hearts+))
         ((and (not (hand-balanced-p hand))
               (>= (hand-suit-length hand +diamonds+) 5))
          (make-bid 3 +diamonds+))
         ((and (not (hand-balanced-p hand))
               (>= (hand-suit-length hand +clubs+) 5))
          (make-bid 3 +clubs+))
         ;; 8+ pts, balanced: 2NT
         (t (make-bid 2 +notrump+))))

      ;; ── Responding to weak two (2♦/2♥/2♠) ──
      ((= p-level 2)
       (cond
         ;; Very strong with support: game
         ((and (>= (hand-suit-length hand p-suit) 3)
               (>= hcp 14))
          (make-bid 4 p-suit))
         ;; Good support (3+ cards) with some values: raise
         ((and (>= (hand-suit-length hand p-suit) 3)
               (>= hcp 10))
          (make-bid 3 p-suit))
         ;; Otherwise pass
         (t nil)))

      ;; ── Responding to preemptive 3-level ──
      ((= p-level 3)
       (cond
         ;; Strong with support: raise to game
         ((and (>= (hand-suit-length hand p-suit) 3)
               (>= hcp 14))
          (if (>= p-suit +hearts+)
              (make-bid 4 p-suit)
              (make-bid 5 p-suit)))
         ;; Otherwise pass
         (t nil)))

      ;; Default: pass
      (t nil))))

(defun ensure-legal-bid (bid current-highest)
  "Ensure a bid is legal (higher than current highest bid).
   If not, return nil (which means pass)."
  (cond
    ((null bid) nil)
    ((not (bid-suit-p bid)) bid)  ; Pass/Double/Redouble always legal (simplified)
    ((null current-highest) bid)  ; No previous bid
    ((bid> bid current-highest) bid)
    (t nil)))  ; Bid too low — force pass

(defun find-partner-opening (bid-history my-position dealer)
  "Find partner's opening bid from the bidding history.
   bid-history is most recent first."
  (let* ((num-bids (length bid-history))
         (partner-pos (partner my-position)))
    (loop for i from (1- num-bids) downto 0
          for bid = (nth i bid-history)
          for bidder = (mod (+ dealer (- num-bids 1 i)) 4)
          when (and (= bidder partner-pos) (bid-suit-p bid))
          return bid)))

(defun find-my-opening (bid-history my-position dealer)
  "Find my own previous bid from the bidding history.
   Returns the first suit bid I made, or nil."
  (let ((num-bids (length bid-history)))
    (loop for i from (1- num-bids) downto 0
          for bid = (nth i bid-history)
          for bidder = (mod (+ dealer (- num-bids 1 i)) 4)
          when (and (= bidder my-position) (bid-suit-p bid))
          return bid)))

(defun hand-ace-count (hand)
  "Count the number of aces in the hand."
  (count-if (lambda (c) (= (card-rank c) 14)) hand))

(defun hand-king-count (hand)
  "Count the number of kings in the hand."
  (count-if (lambda (c) (= (card-rank c) 13)) hand))

(defun stayman-response-p (partner-response my-opening)
  "Is partner's response a Stayman inquiry?
   2♣ after 1NT opening, or 3♣ after 2NT opening."
  (or (and (= (bid-level my-opening) 1)
           (= (bid-suit my-opening) +notrump+)
           (= (bid-level partner-response) 2)
           (= (bid-suit partner-response) +clubs+))
      (and (= (bid-level my-opening) 2)
           (= (bid-suit my-opening) +notrump+)
           (= (bid-level partner-response) 3)
           (= (bid-suit partner-response) +clubs+))))

(defun blackwood-response-p (partner-response)
  "Is partner's bid a Blackwood 4NT (asking for aces after suit bids)?"
  (and (= (bid-level partner-response) 4)
       (= (bid-suit partner-response) +notrump+)))

(defun gerber-response-p (partner-response my-opening)
  "Is partner's bid a Gerber 4♣ (asking for aces after NT bids)?"
  (and (= (bid-level partner-response) 4)
       (= (bid-suit partner-response) +clubs+)
       (= (bid-suit my-opening) +notrump+)))

(defun blackwood-king-ask-p (partner-response)
  "Is partner's bid a Blackwood 5NT (asking for kings)?"
  (and (= (bid-level partner-response) 5)
       (= (bid-suit partner-response) +notrump+)))

(defun gerber-king-ask-p (partner-response)
  "Is partner's bid a Gerber 5♣ (asking for kings)?"
  (and (= (bid-level partner-response) 5)
       (= (bid-suit partner-response) +clubs+)))

(defun ai-opener-rebid (hand my-opening partner-response)
  "Determine opener's rebid after partner has responded.
   Handles Stayman, Blackwood, Gerber, and standard rebids.
   Rules from BRIDGE_BIDDING_DECISIONS.md."
  (let ((hcp (hand-hcp hand))
        (my-suit (bid-suit my-opening))
        (p-suit (bid-suit partner-response))
        (p-level (bid-level partner-response)))

    (cond
      ;; ── Stayman response: partner bid 2♣ after 1NT, or 3♣ after 2NT ──
      ((stayman-response-p partner-response my-opening)
       (let ((response-level (if (= (bid-level my-opening) 1) 2 3)))
         (cond
           ;; Have 4 cards in both majors: bid hearts (per convention)
           ((and (>= (hand-suit-length hand +hearts+) 4)
                 (>= (hand-suit-length hand +spades+) 4))
            (make-bid response-level +hearts+))
           ;; Have 4-card heart suit
           ((>= (hand-suit-length hand +hearts+) 4)
            (make-bid response-level +hearts+))
           ;; Have 4-card spade suit
           ((>= (hand-suit-length hand +spades+) 4)
            (make-bid response-level +spades+))
           ;; No 4-card major: deny with diamonds
           (t (make-bid response-level +diamonds+)))))

      ;; ── Blackwood 4NT: respond with ace count ──
      ((blackwood-response-p partner-response)
       (let ((aces (hand-ace-count hand)))
         ;; 5♣=0, 5♦=1, 5♥=2, 5♠=3, 5NT=4
         (make-bid 5 (mod aces 5))))

      ;; ── Blackwood 5NT: respond with king count ──
      ((blackwood-king-ask-p partner-response)
       (let ((kings (hand-king-count hand)))
         ;; 6♣=0, 6♦=1, 6♥=2, 6♠=3, 6NT=4
         (make-bid 6 (mod kings 5))))

      ;; ── Gerber 4♣ (after NT bids): respond with ace count ──
      ((gerber-response-p partner-response my-opening)
       (let ((aces (hand-ace-count hand)))
         ;; 4♦=0, 4♥=1, 4♠=2, 4NT=3, 5♣=4
         (case aces
           (0 (make-bid 4 +diamonds+))
           (1 (make-bid 4 +hearts+))
           (2 (make-bid 4 +spades+))
           (3 (make-bid 4 +notrump+))
           (4 (make-bid 5 +clubs+)))))

      ;; ── Gerber 5♣ (asking for kings) ──
      ((gerber-king-ask-p partner-response)
       (let ((kings (hand-king-count hand)))
         ;; 5♦=0, 5♥=1, 5♠=2, 5NT=3, 6♣=4
         (case kings
           (0 (make-bid 5 +diamonds+))
           (1 (make-bid 5 +hearts+))
           (2 (make-bid 5 +spades+))
           (3 (make-bid 5 +notrump+))
           (4 (make-bid 6 +clubs+)))))

      ;; ── Partner responded in NT ──
      ((= p-suit +notrump+)
       (cond
         ;; With 6+ card suit, rebid it
         ((and (< my-suit +notrump+)
               (>= (hand-suit-length hand my-suit) 6))
          (make-bid (1+ p-level) my-suit))
         ;; Minimum hand (13-15): pass
         ((<= hcp 15) nil)
         ;; 16-17: raise NT
         ((<= hcp 17) (make-bid (1+ p-level) +notrump+))
         ;; 18+: jump NT
         (t (make-bid (+ p-level 2) +notrump+))))

      ;; ── Partner raised my suit ──
      ((and (< my-suit +notrump+) (= p-suit my-suit))
       (cond
         ;; Minimum: pass
         ((<= hcp 15) nil)
         ;; Slam try with 19+ and major fit
         ((and (>= hcp 19) (>= p-suit +hearts+))
          (make-bid 4 +notrump+))  ; Blackwood
         ;; Game in major
         ((and (>= p-suit +hearts+) (<= hcp 18))
          (make-bid 4 my-suit))
         ;; Game (major or minor)
         (t (make-bid (if (>= p-suit +hearts+) 4 5) my-suit))))

      ;; ── Partner bid a new suit ──
      (t
       (cond
         ;; Support partner's suit
         ((>= (hand-suit-length hand p-suit) 4)
          (make-bid (1+ p-level) p-suit))
         ;; Rebid own 6+ card suit
         ((and (< my-suit +notrump+)
               (>= (hand-suit-length hand my-suit) 6))
          (make-bid 2 my-suit))
         ;; Minimum balanced: bid NT
         ((and (<= hcp 15) (hand-balanced-p hand))
          (make-bid (if (>= p-level 2) p-level 1) +notrump+))
         ;; Rebid own 5+ card suit
         ((and (< my-suit +notrump+)
               (>= (hand-suit-length hand my-suit) 5))
          (make-bid 2 my-suit))
         ;; Fallback: 1NT
         (t (make-bid 1 +notrump+)))))))

(defun ai-select-bid (hand bid-history my-position dealer)
  "Main AI bidding entry point.
   Determines whether to open, respond, rebid, or pass.
   Implements Standard American with slam conventions from
   BRIDGE_BIDDING_DECISIONS.md."
  (let* ((current-highest (last-suit-bid bid-history))
         (partner-bid (find-partner-opening bid-history my-position dealer))
         (my-bid (find-my-opening bid-history my-position dealer))
         (raw-bid
           (cond
             ;; I already bid and partner responded — opener's rebid
             ((and my-bid partner-bid)
              (ai-opener-rebid hand my-bid partner-bid))
             ;; Partner has bid but I haven't — respond
             (partner-bid
              (ai-responding-bid hand partner-bid bid-history my-position dealer))
             ;; No one has bid a suit yet — try to open
             ((null current-highest)
              (ai-opening-bid hand))
             ;; Opponents have bid, partner hasn't — overcall (simplified)
             (t
              (let ((hcp (hand-hcp hand)))
                (cond
                  ((and (>= hcp 13)
                        (hand-has-major-p hand 5))
                   (if (>= (hand-suit-length hand +spades+) 5)
                       (make-bid 1 +spades+)
                       (make-bid 1 +hearts+)))
                  ((and (>= hcp 15) (<= hcp 18)
                        (hand-balanced-p hand))
                   (make-bid 1 +notrump+))
                  (t nil)))))))
    ;; Ensure the bid is legal
    (let ((legal-bid (ensure-legal-bid raw-bid current-highest)))
      (or legal-bid (make-pass)))))
