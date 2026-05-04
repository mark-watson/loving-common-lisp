;;;; engine.lisp — Main Game Engine
;;;;
;;;; Orchestrates a complete game of contract bridge:
;;;; 1. Deal cards
;;;; 2. Bidding phase (AI + human input)
;;;; 3. Playing phase (13 tricks with AI + human input)
;;;; 4. Scoring
;;;;
;;;; Human plays as South; North is AI partner; East-West are AI opponents.

(in-package #:bridge)

;;; ============================================================
;;; GAME STATE
;;; ============================================================

(defstruct game-state
  "Complete state of a bridge game."
  (hands          nil)   ; Array of 4 hands (lists of cards)
  (original-hands nil)   ; Copy of initial deal for review
  (dealer         0)     ; Who dealt (0=N, 1=E, 2=S, 3=W)
  (vulnerability  :none) ; :none, :ns, :ew, :both
  (human-player   +south+)
  ;; Bidding
  (bid-history    nil)   ; List of bids (most recent first)
  (contract       nil)   ; Final contract bid
  (declarer       nil)   ; Declarer player index
  (doubled        0)     ; 0, 1, or 2
  ;; Play
  (dummy          nil)   ; Dummy player index
  (current-trick  nil)   ; Cards played so far in current trick
  (trick-lead     nil)   ; Who leads current trick
  (tricks-ns      0)     ; Tricks won by N-S
  (tricks-ew      0)     ; Tricks won by E-W
  (tricks-played  0)     ; Total tricks completed
  (cards-played   nil)   ; All cards played so far
  (trump-suit     nil)   ; Trump suit (nil = NT)
  ;; Phase
  (phase          :dealing)) ; :dealing, :bidding, :playing, :scoring, :done

;;; ============================================================
;;; DEALING
;;; ============================================================

(defun new-game (&key (dealer +north+) (vulnerability :none)
                       (human-player +south+))
  "Create and deal a new game."
  (let* ((deck (shuffle-deck (make-deck)))
         (hands (deal-hands deck))
         (gs (make-game-state
              :hands hands
              :original-hands (map 'vector #'copy-list hands)
              :dealer dealer
              :vulnerability vulnerability
              :human-player human-player
              :phase :bidding)))
    gs))

;;; ============================================================
;;; HUMAN INPUT
;;; ============================================================

(defun parse-bid-input (input)
  "Parse a bid string like '1H', '3NT', 'PASS', 'DBL', etc."
  (let ((s (string-upcase (string-trim " " input))))
    (cond
      ((or (string= s "P") (string= s "PASS"))
       (make-pass))
      ((or (string= s "D") (string= s "DBL") (string= s "DOUBLE") (string= s "X"))
       (make-double-bid))
      ((or (string= s "R") (string= s "RDBL") (string= s "REDOUBLE") (string= s "XX"))
       (make-redouble-bid))
      (t
       (when (>= (length s) 2)
         (let ((level (digit-char-p (char s 0)))
               (strain-str (subseq s 1)))
           (when (and level (>= level 1) (<= level 7))
             (let ((suit (cond
                           ((or (string= strain-str "C") (string= strain-str "♣")
                                (string= strain-str "CLUBS"))
                            +clubs+)
                           ((or (string= strain-str "D") (string= strain-str "♦")
                                (string= strain-str "DIAMONDS"))
                            +diamonds+)
                           ((or (string= strain-str "H") (string= strain-str "♥")
                                (string= strain-str "HEARTS"))
                            +hearts+)
                           ((or (string= strain-str "S") (string= strain-str "♠")
                                (string= strain-str "SPADES"))
                            +spades+)
                           ((or (string= strain-str "N") (string= strain-str "NT")
                                (string= strain-str "NOTRUMP") (string= strain-str "NO TRUMP"))
                            +notrump+)
                           (t nil))))
               (when suit
                 (make-bid level suit))))))))))

(defun parse-card-input (input hand)
  "Parse a card string like 'AS', 'KH', '10C', '3D' and find it in hand.
   Also accepts numbered index like '1', '2', etc."
  (let ((s (string-upcase (string-trim " " input))))
    ;; Try as index first — only if the entire string is numeric
    (when (every #'digit-char-p s)
      (let ((idx (parse-integer s :junk-allowed t)))
        (when (and idx (>= idx 1) (<= idx (length hand)))
          (return-from parse-card-input (nth (1- idx) hand)))))

    ;; Parse as card name
    (when (>= (length s) 2)
      (let* ((suit-char (char s (1- (length s))))
             (rank-str (subseq s 0 (1- (length s))))
             (suit (case suit-char
                     (#\S +spades+) (#\H +hearts+)
                     (#\D +diamonds+) (#\C +clubs+)
                     (t nil)))
             (rank (cond
                     ((string= rank-str "A") 14)
                     ((string= rank-str "K") 13)
                     ((string= rank-str "Q") 12)
                     ((string= rank-str "J") 11)
                     (t (parse-integer rank-str :junk-allowed t)))))
        (when (and suit rank (>= rank 2) (<= rank 14))
          (find-if (lambda (c) (and (= (card-suit c) suit) (= (card-rank c) rank)))
                   hand))))))

(defun quit-input-p (input)
  "Check if the user wants to quit."
  (member (string-upcase (string-trim " " input))
          '("Q" "QUIT") :test #'string=))

(defun prompt-for-bid (gs)
  "Prompt the human player for a bid."
  (let ((current-highest (last-suit-bid (game-state-bid-history gs))))
    (format t "~%  Your hand:~%")
    (hand-summary (aref (game-state-hands gs) (game-state-human-player gs)))
    (format t "~%  Current highest bid: ~A~%"
            (if current-highest current-highest "None"))
    (format t "  Enter bid (e.g. 1H, 2NT, 3S, PASS, DBL, Q=quit): ")
    (force-output)
    (let* ((input (read-line)))
      (when (quit-input-p input)
        (format t "~%  Quitting game...~%")
        (throw 'quit-game nil))
      (let ((bid (parse-bid-input input)))
        (cond
          ((null bid)
           (format t "  Invalid bid. Try again.~%")
           (prompt-for-bid gs))
          ;; Verify bid is legal
          ((and (bid-suit-p bid) current-highest (not (bid> bid current-highest)))
           (format t "  Bid must be higher than ~A. Try again.~%" current-highest)
           (prompt-for-bid gs))
          (t bid))))))

(defun prompt-for-card (gs player &optional lead-suit)
  "Prompt the human player to select a card.
   lead-suit is the suit that was led (nil if this player is leading)."
  (let* ((hand (aref (game-state-hands gs) player))
         (legal (legal-plays hand lead-suit))
         (is-dummy (= player (game-state-dummy gs))))

    ;; Show whose cards we're playing
    (when is-dummy
      (format t "~%  >>> Playing DUMMY's hand (~A) <<<~%"
              (aref +player-names+ player)))

    ;; Show full hand organized by suit
    (format t "~%  ~A hand:~%" (if is-dummy "Dummy's" "Your"))
    (display-hand-by-suit hand)

    ;; Show numbered legal plays for selection
    (cond
      ((null lead-suit)
       (format t "~%  ~A leading — select any card:~%"
               (if is-dummy "Dummy is" "You are")))
      ((some (lambda (c) (= (card-suit c) lead-suit)) hand)
       (format t "~%  Must follow ~A — legal plays:~%"
               (aref +suit-names+ lead-suit)))
      (t
       (format t "~%  No ~A — play any card:~%"
               (aref +suit-names+ lead-suit))))
    (loop for card in legal
          for i from 1
          do (format t "    ~D. ~A  (~A)~%"
                     i card (card-name card)))

    (format t "  Select card (number or name like AS, KH, 10C, Q=quit): ")
    (force-output)
    (let* ((input (read-line)))
      (when (quit-input-p input)
        (format t "~%  Quitting game...~%")
        (throw 'quit-game nil))
      (let ((card (parse-card-input input legal)))
        (cond
          ((null card)
           (format t "  Invalid card. Try again.~%")
           (prompt-for-card gs player lead-suit))
          ((not (member card legal :test #'card=))
           (format t "  That card is not a legal play. Try again.~%")
           (prompt-for-card gs player lead-suit))
          (t card))))))

;;; ============================================================
;;; BIDDING PHASE
;;; ============================================================

(defun run-bidding-phase (gs)
  "Run the complete bidding phase. Returns updated game-state."
  (format t "~%═══════════════════════════════════════~%")
  (format t " BIDDING PHASE  (Dealer: ~A)~%"
          (aref +player-names+ (game-state-dealer gs)))
  (format t "═══════════════════════════════════════~%")

  ;; Show the human's hand
  (format t "~%  Your hand (South):~%")
  (hand-summary (aref (game-state-hands gs) (game-state-human-player gs)))

  (let ((current-bidder (game-state-dealer gs))
        (bid-history nil)
        (pass-count 0)
        (any-suit-bid nil))

    (loop
      ;; Check if bidding is done
      (when (and any-suit-bid (>= pass-count 3))
        (return))
      (when (and (not any-suit-bid) (>= pass-count 4))
        (return))

      ;; Get bid
      (let ((bid (if (= current-bidder (game-state-human-player gs))
                     ;; Human bids
                     (prompt-for-bid gs)
                     ;; AI bids
                     (let ((ai-bid (ai-select-bid
                                    (aref (game-state-hands gs) current-bidder)
                                    bid-history current-bidder
                                    (game-state-dealer gs))))
                       (format t "  ~A bids: ~A~%"
                               (aref +player-names+ current-bidder) ai-bid)
                       ai-bid))))

        ;; Record bid
        (push bid bid-history)
        (setf (game-state-bid-history gs) bid-history)

        ;; Track passes
        (if (bid-pass-p bid)
            (incf pass-count)
            (setf pass-count 0))
        (when (bid-suit-p bid)
          (setf any-suit-bid t))

        ;; Next bidder
        (setf current-bidder (next-player current-bidder))))

    ;; Display bidding history
    (display-bidding-history bid-history (game-state-dealer gs))

    ;; Extract contract
    (if (passed-out-p bid-history)
        (progn
          (format t "~%  *** PASSED OUT — no contract ***~%")
          (setf (game-state-phase gs) :done))
        (multiple-value-bind (contract declarer doubled)
            (extract-contract bid-history (game-state-dealer gs))
          (setf (game-state-contract gs) contract
                (game-state-declarer gs) declarer
                (game-state-doubled gs) doubled
                (game-state-dummy gs) (partner declarer)
                (game-state-trump-suit gs) (when (< (bid-suit contract) +notrump+)
                                              (bid-suit contract))
                (game-state-trick-lead gs) (next-player declarer)
                (game-state-phase gs) :playing)

          (display-contract contract declarer doubled)
          (format t "  Dummy: ~A~%"
                  (aref +player-names+ (game-state-dummy gs)))

          ;; If North wins the bid, swap N/S cards so the human
          ;; (always seated at South) plays North's original hand.
          ;; South's original cards move to the North (dummy) position.
          ;; Also update dummy designation: North is now the dummy.
          (when (and (= (game-state-human-player gs) +south+)
                     (= declarer +north+))
            (let ((north-cards (aref (game-state-hands gs) +north+))
                  (south-cards (aref (game-state-hands gs) +south+)))
              (setf (aref (game-state-hands gs) +south+) north-cards
                    (aref (game-state-hands gs) +north+) south-cards))
            ;; Swap dummy designation: North now holds the dummy cards
            (setf (game-state-dummy gs) +north+)
            (format t "~%  *** North won the contract! ***~%")
            (format t "  Your cards have been swapped — you now play as declarer.~%")
            (format t "  North (dummy) now holds your original cards (face up).~%"))))

    gs))

;;; ============================================================
;;; PLAYING PHASE
;;; ============================================================

(defun run-playing-phase (gs)
  "Run the complete playing phase (13 tricks). Returns updated game-state."
  (format t "~%═══════════════════════════════════════~%")
  (format t " PLAYING PHASE~%")
  (display-contract (game-state-contract gs)
                    (game-state-declarer gs)
                    (game-state-doubled gs))
  (format t "═══════════════════════════════════════~%")

  ;; Show the board
  (display-board (game-state-hands gs)
                 :human-player (game-state-human-player gs)
                 :dummy (game-state-dummy gs)
                 :vulnerability (game-state-vulnerability gs))

  ;; Play 13 tricks
  (loop for trick-num from 1 to 13
        do (progn
             (format t "~%── Trick ~D ──  " trick-num)
             (display-trick-count (game-state-tricks-ns gs)
                                  (game-state-tricks-ew gs))

             (let ((trick nil)
                   (lead-suit nil)
                   (leader (game-state-trick-lead gs)))

               ;; Each player plays one card
               (loop for i from 0 below 4
                     for player = (mod (+ leader i) 4)
                     do (let* ((hand (aref (game-state-hands gs) player))
                               (is-human (= player (game-state-human-player gs)))
                               (is-dummy (= player (game-state-dummy gs)))
                               ;; Human controls dummy when their side declares
                               (human-side (mod (game-state-human-player gs) 2))
                               (decl-side (mod (game-state-declarer gs) 2))
                               (ns-declares (= human-side decl-side))
                               (human-plays-this (or is-human
                                                     (and is-dummy ns-declares)))
                               (card
                                 (cond
                                   ;; Human plays their own hand or dummy
                                   (human-plays-this
                                    ;; Show current state
                                    (when trick
                                      (display-current-trick (reverse trick) leader))
                                    (prompt-for-card gs player lead-suit))
                                   ;; AI plays
                                   (t
                                    (let ((c (ai-select-card
                                              hand trick leader lead-suit
                                              (game-state-trump-suit gs)
                                              player
                                              (game-state-declarer gs)
                                              (game-state-tricks-ns gs)
                                              (game-state-tricks-ew gs)
                                              (game-state-tricks-played gs))))
                                      (format t "  ~A~A plays: ~A~%"
                                              (aref +player-names+ player)
                                              (if is-dummy " (Dummy)" "")
                                              c)
                                      c)))))
                          ;; Record card
                          (push card trick)
                          (when (null lead-suit)
                            (setf lead-suit (card-suit card)))
                          ;; Remove from hand
                          (setf (aref (game-state-hands gs) player)
                                (remove card hand :test #'card= :count 1))
                          ;; Track all played cards
                          (push card (game-state-cards-played gs))))

               ;; Reverse trick to play order
               (setf trick (nreverse trick))

               ;; Determine winner
               (let ((winner (trick-winner trick leader
                                           (game-state-trump-suit gs))))
                 (format t "  → ~A wins the trick~%"
                         (aref +player-names+ winner))

                 ;; Score trick
                 (if (evenp winner) ; N-S
                     (incf (game-state-tricks-ns gs))
                     (incf (game-state-tricks-ew gs)))
                 (incf (game-state-tricks-played gs))

                 ;; Winner leads next
                 (setf (game-state-trick-lead gs) winner)))))

  ;; Playing complete
  (setf (game-state-phase gs) :scoring)

  (format t "~%═══════════════════════════════════════~%")
  (format t " ALL TRICKS PLAYED~%")
  (display-trick-count (game-state-tricks-ns gs) (game-state-tricks-ew gs))
  (format t "═══════════════════════════════════════~%")

  gs)

;;; ============================================================
;;; SCORING PHASE
;;; ============================================================

(defun run-scoring-phase (gs &optional rubber-state)
  "Calculate and display the score for a completed deal.
   If rubber-state is provided, updates it for rubber bridge."
  (let* ((declarer (game-state-declarer gs))
         (contract (game-state-contract gs))
         (level (bid-level contract))
         (suit (bid-suit contract))
         (tricks-won (if (evenp declarer)
                         (game-state-tricks-ns gs)
                         (game-state-tricks-ew gs)))
         (doubled (game-state-doubled gs)))

    (if rubber-state
        ;; Rubber bridge scoring
        (let ((deal-score (score-rubber-deal level suit tricks-won declarer
                                             doubled rubber-state)))
          (display-deal-result level suit tricks-won declarer doubled deal-score)
          (display-rubber-score rubber-state)

          ;; Check if a game was just won
          (when (or (rubber-state-ns-vulnerable rubber-state)
                    (rubber-state-ew-vulnerable rubber-state))
            (format t "~%  *** GAME! ***~%"))

          deal-score)
        ;; Fallback: simple scoring
        (let* ((score (score-rubber-deal level suit tricks-won declarer
                                          doubled (make-rubber-state))))
          (display-score score declarer)))

    (setf (game-state-phase gs) :done)
    gs))

;;; ============================================================
;;; MAIN GAME LOOP — RUBBER BRIDGE
;;; ============================================================

(defun play-bridge ()
  "Play a complete rubber of bridge (first to 2 games wins).
   Type Q or QUIT at any prompt to exit."
  (display-welcome)

  (let ((rs (make-rubber-state)))
    (catch 'quit-game
      (loop while (not (rubber-complete-p rs))
            do (let* ((dealer (rubber-state-current-dealer rs))
                      (vulnerability (rubber-vulnerability rs))
                      (gs (new-game :dealer dealer
                                    :vulnerability vulnerability
                                    :human-player +south+)))

                 (incf (rubber-state-deals-played rs))
                 (format t "~%~%══════════════════════════════════════════~%")
                 (format t "  DEAL #~D   (N-S games: ~D  E-W games: ~D)~%"
                         (rubber-state-deals-played rs)
                         (rubber-state-ns-games rs)
                         (rubber-state-ew-games rs))
                 (format t "══════════════════════════════════════════~%")

                 ;; Bidding
                 (setf gs (run-bidding-phase gs))

                 ;; If passed out, skip to next deal
                 (if (eq (game-state-phase gs) :done)
                     (format t "~%  Deal passed out. Rotating dealer...~%")
                     (progn
                       ;; Playing
                       (setf gs (run-playing-phase gs))
                       ;; Scoring (updates rubber state)
                       (run-scoring-phase gs rs)))

                 ;; Rotate dealer
                 (setf (rubber-state-current-dealer rs)
                       (next-player (rubber-state-current-dealer rs)))))

      ;; Rubber complete!
      (display-rubber-final rs))
    (format t "~%  Thanks for playing!~%")
    rs))

;;; ============================================================
;;; AUTO-PLAY (for testing / demonstration)
;;; ============================================================

(defun auto-play (&key (dealer +north+) (vulnerability :none) (verbose t))
  "Play a complete game with all AI players (no human input).
   Useful for testing the engine and AI."
  (let* ((deck (shuffle-deck (make-deck)))
         (hands (deal-hands deck))
         (gs (make-game-state
              :hands hands
              :original-hands (map 'vector #'copy-list hands)
              :dealer dealer
              :vulnerability vulnerability
              :human-player -1  ; No human
              :phase :bidding)))

    (when verbose
      (format t "~%═══ AUTO-PLAY GAME ═══~%")
      (format t "~%All hands:~%")
      (loop for p from 0 below 4
            do (format t "~%~A:~%" (aref +player-names+ p))
               (hand-summary (aref (game-state-original-hands gs) p))))

    ;; Bidding phase (all AI)
    (let ((current-bidder dealer)
          (bid-history nil)
          (pass-count 0)
          (any-suit-bid nil))

      (loop
        (when (and any-suit-bid (>= pass-count 3)) (return))
        (when (and (not any-suit-bid) (>= pass-count 4)) (return))

        (let ((bid (ai-select-bid
                    (aref (game-state-hands gs) current-bidder)
                    bid-history current-bidder dealer)))
          (when verbose
            (format t "  ~A: ~A~%"
                    (aref +player-names+ current-bidder) bid))
          (push bid bid-history)
          (if (bid-pass-p bid) (incf pass-count) (setf pass-count 0))
          (when (bid-suit-p bid) (setf any-suit-bid t))
          (setf current-bidder (next-player current-bidder))))

      (setf (game-state-bid-history gs) bid-history)

      (when (passed-out-p bid-history)
        (when verbose (format t "~%Passed out.~%"))
        (return-from auto-play gs))

      (multiple-value-bind (contract declarer doubled)
          (extract-contract bid-history dealer)
        (setf (game-state-contract gs) contract
              (game-state-declarer gs) declarer
              (game-state-doubled gs) doubled
              (game-state-dummy gs) (partner declarer)
              (game-state-trump-suit gs) (when (< (bid-suit contract) +notrump+)
                                            (bid-suit contract))
              (game-state-trick-lead gs) (next-player declarer)
              (game-state-phase gs) :playing)

        (when verbose
          (display-contract contract declarer doubled))

        ;; Playing phase (all AI)
        (loop for trick-num from 1 to 13
              do (let ((trick nil)
                       (lead-suit nil)
                       (leader (game-state-trick-lead gs)))
                   (loop for i from 0 below 4
                         for player = (mod (+ leader i) 4)
                         for hand = (aref (game-state-hands gs) player)
                         for card = (ai-select-card
                                     hand trick leader lead-suit
                                     (game-state-trump-suit gs)
                                     player declarer
                                     (game-state-tricks-ns gs)
                                     (game-state-tricks-ew gs)
                                     (game-state-tricks-played gs))
                         do (progn
                              (push card trick)
                              (when (null lead-suit)
                                (setf lead-suit (card-suit card)))
                              (setf (aref (game-state-hands gs) player)
                                    (remove card hand :test #'card= :count 1))
                              (push card (game-state-cards-played gs))))
                   (setf trick (nreverse trick))
                   (let ((winner (trick-winner trick leader
                                               (game-state-trump-suit gs))))
                     (when verbose
                       (format t "  Trick ~2D: ~{~A~^ ~} → ~A~%"
                               trick-num trick
                               (aref +player-names+ winner)))
                     (if (evenp winner)
                         (incf (game-state-tricks-ns gs))
                         (incf (game-state-tricks-ew gs)))
                     (incf (game-state-tricks-played gs))
                     (setf (game-state-trick-lead gs) winner))))

        ;; Scoring
        (let* ((tricks-won (if (evenp declarer)
                               (game-state-tricks-ns gs)
                               (game-state-tricks-ew gs)))
               (rs (make-rubber-state))
               (score (score-rubber-deal (bid-level contract)
                                          (bid-suit contract)
                                          tricks-won declarer
                                          doubled rs)))
          (when verbose
            (format t "~%  N-S: ~D tricks, E-W: ~D tricks~%"
                    (game-state-tricks-ns gs) (game-state-tricks-ew gs))
            (format t "  Score: ~D~%" score))
          (setf (game-state-phase gs) :done)
          gs)))))
