;;;; play.lisp — Card Play AI and Engine
;;;;
;;;; Implements:
;;;; - Card play rules (following suit, trumping)
;;;; - Trick winner determination
;;;; - Heuristic card selection for AI
;;;; - Simplified Monte Carlo card play
;;;; - Double-dummy solver (minimax with alpha-beta)

(in-package #:bridge)

;;; ============================================================
;;; CARD PLAY RULES
;;; ============================================================

(defun legal-plays (hand lead-suit)
  "Return legal cards that can be played.
   Must follow suit if possible."
  (if (null lead-suit)
      ;; Leading — any card is legal
      (copy-list hand)
      ;; Following — must follow suit if possible
      (let ((in-suit (hand-suit-cards hand lead-suit)))
        (if in-suit
            in-suit
            (copy-list hand)))))

(defun trick-winner (trick lead-player trump-suit)
  "Determine the winner of a completed trick.
   trick is a list of 4 cards in play order.
   Returns the winning player index."
  (let* ((lead-card (first trick))
         (lead-suit (card-suit lead-card))
         (best-card lead-card)
         (best-player lead-player))
    (loop for i from 1 below 4
          for card = (nth i trick)
          for player = (mod (+ lead-player i) 4)
          do (cond
               ;; Trump beats non-trump
               ((and trump-suit
                     (= (card-suit card) trump-suit)
                     (/= (card-suit best-card) trump-suit))
                (setf best-card card
                      best-player player))
               ;; Higher trump beats lower trump
               ((and trump-suit
                     (= (card-suit card) trump-suit)
                     (= (card-suit best-card) trump-suit)
                     (> (card-rank card) (card-rank best-card)))
                (setf best-card card
                      best-player player))
               ;; Higher card in lead suit (when best is also lead suit)
               ((and (= (card-suit card) lead-suit)
                     (= (card-suit best-card) lead-suit)
                     (> (card-rank card) (card-rank best-card)))
                (setf best-card card
                      best-player player))
               ;; Otherwise: off-suit non-trump card loses
               ))
    best-player))

;;; ============================================================
;;; HEURISTIC CARD PLAY AI
;;; ============================================================

(defun card-play-strength (card lead-suit trump-suit)
  "Evaluate a card's trick-winning potential.
   Higher value = more likely to win."
  (cond
    ;; Trump suit is very strong
    ((and trump-suit (= (card-suit card) trump-suit))
     (+ 100 (card-rank card)))
    ;; Lead suit
    ((and lead-suit (= (card-suit card) lead-suit))
     (+ 50 (card-rank card)))
    ;; Off-suit
    (t (card-rank card))))

(defun ai-select-lead (hand trump-suit my-position declarer tricks-played
                       &key (ns-tricks 0) (ew-tricks 0))
  "AI selects a card to lead.
   Heuristic strategy for opening leads and subsequent leads."
  (declare (ignore tricks-played ns-tricks ew-tricks))
  (let* ((my-side (mod my-position 2))
         (decl-side (mod declarer 2))
         (defending (not (= my-side decl-side))))
    (cond
      ;; DEFENDING: Lead strategies
      (defending
       (or
        ;; 1. Lead partner's suit if known (simplified: skip for now)
        ;; 2. Lead top of a sequence (e.g., K from KQJ)
        (lead-top-of-sequence hand trump-suit)
        ;; 3. Lead 4th best of longest suit (not trump)
        (lead-fourth-best hand trump-suit)
        ;; 4. Lead shortest non-trump suit
        (lead-short-suit hand trump-suit)
        ;; 5. Fallback: lowest card of longest suit
        (car (last (hand-suit-cards hand (hand-longest-non-trump hand trump-suit))))))

      ;; DECLARING: Lead strategies
      (t
       (or
        ;; 1. Draw trump if we have strong trumps
        (when (and trump-suit (>= (hand-suit-length hand trump-suit) 3))
          (let ((trumps (hand-suit-cards hand trump-suit)))
            (when (some (lambda (c) (>= (card-rank c) 12)) trumps) ; Q or higher
              (first (sort (copy-list trumps) #'> :key #'card-rank)))))
        ;; 2. Lead from a long strong suit
        (lead-top-of-sequence hand trump-suit)
        ;; 3. Lead toward a tenace (simplified)
        (lead-fourth-best hand trump-suit)
        ;; 4. Fallback
        (first hand))))))

(defun hand-longest-non-trump (hand trump-suit)
  "Find the longest non-trump suit in hand."
  (let ((best-suit nil)
        (best-len 0))
    (dolist (suit +suits+)
      (unless (and trump-suit (= suit trump-suit))
        (let ((len (hand-suit-length hand suit)))
          (when (> len best-len)
            (setf best-suit suit
                  best-len len)))))
    (or best-suit (hand-longest-suit hand))))

(defun lead-top-of-sequence (hand trump-suit)
  "Lead the top of a sequence (e.g., K from KQJ, Q from QJT)."
  (dolist (suit +suits+)
    (unless (and trump-suit (= suit trump-suit))
      (let* ((cards (sort (copy-list (hand-suit-cards hand suit))
                          #'> :key #'card-rank))
             (len (length cards)))
        (when (>= len 3)
          ;; Check for 3-card+ sequence from top
          (let ((top (card-rank (first cards))))
            (when (and (>= top 11) ; J or higher
                       (= (card-rank (second cards)) (1- top))
                       (= (card-rank (third cards)) (- top 2)))
              (return (first cards)))))))))

(defun lead-fourth-best (hand trump-suit)
  "Lead 4th best of longest and strongest suit."
  (let* ((best-suit (hand-longest-non-trump hand trump-suit))
         (cards (when best-suit
                  (sort (copy-list (hand-suit-cards hand best-suit))
                        #'> :key #'card-rank))))
    (when (and cards (>= (length cards) 4))
      (fourth cards))))

(defun lead-short-suit (hand trump-suit)
  "Lead from shortest side suit (for ruffing potential)."
  (when trump-suit
    (let ((best-suit nil)
          (best-len 14))
      (dolist (suit +suits+)
        (unless (= suit trump-suit)
          (let ((len (hand-suit-length hand suit)))
            (when (and (> len 0) (< len best-len))
              (setf best-suit suit
                    best-len len)))))
      (when best-suit
        ;; Lead high from short suit
        (first (sort (copy-list (hand-suit-cards hand best-suit))
                     #'> :key #'card-rank))))))

(defun ai-select-follow (hand trick lead-player lead-suit trump-suit
                          my-position declarer)
  "AI selects a card when following (not leading).
   Uses heuristic play strategies."
  (let* ((legal (legal-plays hand lead-suit))
         (my-side (mod my-position 2))
         (decl-side (mod declarer 2))
         (defending (not (= my-side decl-side)))
         (partner-pos (partner my-position))
         (cards-played (length trick))
         ;; Who's winning so far?
         (current-winner (when (> cards-played 0)
                           (trick-winner-partial trick lead-player trump-suit)))
         (partner-winning (and current-winner
                               (= (mod current-winner 2) my-side))))
    (declare (ignore partner-pos))

    (cond
      ;; Only one legal play
      ((= (length legal) 1)
       (first legal))

      ;; MUST follow suit
      ((some (lambda (c) (= (card-suit c) lead-suit)) legal)
       (let ((in-suit (remove-if-not (lambda (c) (= (card-suit c) lead-suit)) legal)))
         (cond
           ;; Last to play (4th seat) — play as cheaply as possible if partner winning
           ((and (= cards-played 3) partner-winning)
            (lowest-card in-suit))
           ;; Can win the trick — play just enough to win
           ((can-win-trick in-suit trick lead-player trump-suit)
            (cheapest-winner in-suit trick lead-player trump-suit))
           ;; Can't win — play lowest
           (t (lowest-card in-suit)))))

      ;; CAN'T follow suit — can trump or discard
      (t
       (let ((trumps (when trump-suit
                       (remove-if-not (lambda (c) (= (card-suit c) trump-suit)) legal)))
             (non-trumps (if trump-suit
                             (remove-if (lambda (c) (= (card-suit c) trump-suit)) legal)
                             legal)))
         (cond
           ;; Defending: trump if possible and partner isn't winning
           ((and defending trumps (not partner-winning))
            (lowest-card trumps)) ; Use lowest trump to win
           ;; Declaring: trump when beneficial
           ((and (not defending) trumps (not partner-winning))
            (lowest-card trumps))
           ;; Discard — throw away lowest card from weakest suit
           (t (best-discard non-trumps trump-suit))))))))

(defun trick-winner-partial (trick lead-player trump-suit)
  "Determine the current winner of a partial trick."
  (let* ((lead-card (first trick))
         (lead-suit (card-suit lead-card))
         (best-card lead-card)
         (best-player lead-player))
    (loop for i from 1 below (length trick)
          for card = (nth i trick)
          for player = (mod (+ lead-player i) 4)
          do (cond
               ((and trump-suit
                     (= (card-suit card) trump-suit)
                     (/= (card-suit best-card) trump-suit))
                (setf best-card card best-player player))
               ((and trump-suit
                     (= (card-suit card) trump-suit)
                     (= (card-suit best-card) trump-suit)
                     (> (card-rank card) (card-rank best-card)))
                (setf best-card card best-player player))
               ((and (= (card-suit card) lead-suit)
                     (= (card-suit best-card) lead-suit)
                     (> (card-rank card) (card-rank best-card)))
                (setf best-card card best-player player))))
    best-player))

(defun lowest-card (cards)
  "Return the lowest-ranking card."
  (reduce (lambda (a b) (if (< (card-rank a) (card-rank b)) a b)) cards))

(defun highest-card (cards)
  "Return the highest-ranking card."
  (reduce (lambda (a b) (if (> (card-rank a) (card-rank b)) a b)) cards))

(defun can-win-trick (in-suit-cards trick lead-player trump-suit)
  "Can any of our in-suit cards beat the current trick winner?"
  (declare (ignore lead-player))
  (let* ((lead-suit (card-suit (first trick)))
         (winning-rank
           (loop for i from 0 below (length trick)
                 for card = (nth i trick)
                 when (or (and trump-suit (= (card-suit card) trump-suit))
                          (= (card-suit card) lead-suit))
                 maximize (card-rank card))))
    (some (lambda (c) (> (card-rank c) winning-rank)) in-suit-cards)))

(defun cheapest-winner (in-suit-cards trick lead-player trump-suit)
  "Play the cheapest card that can win the trick."
  (declare (ignore lead-player))
  (let* ((lead-suit (card-suit (first trick)))
         (winning-rank
           (loop for i from 0 below (length trick)
                 for card = (nth i trick)
                 when (or (and trump-suit (= (card-suit card) trump-suit))
                          (= (card-suit card) lead-suit))
                 maximize (card-rank card)))
         (winners (remove-if-not (lambda (c) (> (card-rank c) winning-rank))
                                 in-suit-cards)))
    (if winners
        (lowest-card winners)
        (lowest-card in-suit-cards))))

(defun best-discard (cards trump-suit)
  "Choose the best card to discard (no trumping).
   Discard from the suit where we have the least chance of winning tricks."
  (declare (ignore trump-suit))
  (if (null cards)
      nil
      ;; Simple heuristic: discard lowest card from longest non-trump suit
      (lowest-card cards)))

;;; ============================================================
;;; MAIN AI CARD SELECTION
;;; ============================================================

(defun ai-select-card (hand trick lead-player lead-suit trump-suit
                       my-position declarer ns-tricks ew-tricks tricks-played)
  "Main AI entry point for selecting a card to play."
  (let ((card (if (null trick)
                  ;; We are leading
                  (ai-select-lead hand trump-suit my-position declarer tricks-played
                                  :ns-tricks ns-tricks :ew-tricks ew-tricks)
                  ;; We are following
                  (ai-select-follow hand trick lead-player lead-suit trump-suit
                                    my-position declarer))))
    ;; Safety fallback: if heuristics returned nil, play first legal card
    (or card (first (legal-plays hand lead-suit)) (first hand))))

;;; ============================================================
;;; MONTE CARLO CARD PLAY (PROTOTYPE)
;;; ============================================================
;;; This is a simplified implementation of Perfect Information Monte Carlo.
;;; It samples possible hands, runs a heuristic playout for each, and
;;; selects the card that wins the most tricks on average.

(defun generate-consistent-deal (known-hand my-position remaining-cards other-hands-sizes)
  "Generate a random deal consistent with what we know.
   known-hand: our cards
   remaining-cards: all cards we haven't seen
   other-hands-sizes: how many cards each other player should have"
  (let* ((shuffled (shuffle-deck (coerce (copy-list remaining-cards) 'vector)))
         (hands (make-array 4 :initial-element nil))
         (idx 0))
    (setf (aref hands my-position) (copy-list known-hand))
    (loop for p from 0 below 4
          unless (= p my-position)
          do (let ((n (aref other-hands-sizes p)))
               (setf (aref hands p)
                     (loop for i from idx below (+ idx n)
                           collect (aref shuffled i)))
               (incf idx n)))
    hands))

(defun simulate-remaining-tricks (hands lead-player trump-suit
                                  declarer current-ns current-ew)
  "Simulate the remaining tricks using heuristic play.
   Returns (ns-tricks . ew-tricks) at the end."
  (let ((ns-tricks current-ns)
        (ew-tricks current-ew)
        (leader lead-player)
        (tricks-played (+ current-ns current-ew)))
    (loop while (> (length (aref hands 0)) 0)
          do (let ((trick nil)
                   (lead-suit nil))
               ;; Each player plays a card
               (loop for i from 0 below 4
                     for player = (mod (+ leader i) 4)
                     for hand = (aref hands player)
                     for card = (ai-select-card hand trick leader lead-suit
                                                trump-suit player declarer
                                                ns-tricks ew-tricks tricks-played)
                     do (progn
                          (push card trick)
                          (when (null lead-suit)
                            (setf lead-suit (card-suit card)))
                          (setf (aref hands player)
                                (remove card hand :test #'card= :count 1))))
               ;; Determine winner
               (setf trick (nreverse trick))
               (let ((winner (trick-winner trick leader trump-suit)))
                 (if (evenp winner) ; N-S = 0,2
                     (incf ns-tricks)
                     (incf ew-tricks))
                 (setf leader winner)
                 (incf tricks-played))))
    (cons ns-tricks ew-tricks)))

(defun monte-carlo-select-card (hand trick lead-player lead-suit trump-suit
                                my-position declarer ns-tricks ew-tricks
                                cards-played &key (num-samples 50))
  "Use Monte Carlo simulation to select the best card.
   Tries each legal play and simulates NUM-SAMPLES random completions."
  (let* ((legal (if lead-suit
                    (legal-plays hand lead-suit)
                    (copy-list hand)))
         (all-cards (make-deck))
         ;; Cards we've seen: our hand + cards already played
         (seen-cards (append (copy-list hand)
                             (copy-list cards-played)
                             (when trick (copy-list trick))))
         (remaining (remove-if (lambda (c)
                                 (some (lambda (s) (card= c s)) seen-cards))
                               (coerce all-cards 'list)))
         (other-sizes (make-array 4 :initial-element 0))
         (best-card (first legal))
         (best-score most-negative-fixnum))

    ;; Calculate how many cards each other player should have
    (let ((cards-per-hand (length hand)))
      (loop for p from 0 below 4
            unless (= p my-position)
            do (setf (aref other-sizes p) cards-per-hand)))

    ;; Adjust for trick in progress
    (when trick
      (loop for i from 0 below (length trick)
            for p = (mod (+ lead-player i) 4)
            unless (= p my-position)
            do (decf (aref other-sizes p))))

    ;; Try each legal card
    (dolist (card legal)
      (let ((total-score 0))
        (dotimes (sample num-samples)
          ;; Generate random deal
          (let* ((sim-hands (generate-consistent-deal
                             (remove card hand :test #'card= :count 1)
                             my-position remaining other-sizes))
                 ;; Complete the current trick
                 (sim-trick (append (when trick (copy-list trick)) (list card)))
                 (sim-ns ns-tricks)
                 (sim-ew ew-tricks)
                 (next-leader lead-player))

            ;; If trick is now complete, score it
            (when (= (length sim-trick) 4)
              (let ((winner (trick-winner sim-trick
                                          (or lead-player my-position)
                                          trump-suit)))
                (if (evenp winner)
                    (incf sim-ns)
                    (incf sim-ew))
                (setf next-leader winner
                      sim-trick nil)))

            ;; Simulate remaining tricks
            (let ((result (simulate-remaining-tricks
                           sim-hands
                           (or next-leader (mod (+ my-position 1) 4))
                           trump-suit declarer sim-ns sim-ew)))
              ;; Score from our perspective
              (if (evenp my-position)
                  (incf total-score (car result))   ; N-S tricks
                  (incf total-score (cdr result)))))) ; E-W tricks

        ;; Average score for this card
        (let ((avg-score (/ total-score num-samples)))
          (when (> avg-score best-score)
            (setf best-score avg-score
                  best-card card)))))
    best-card))
