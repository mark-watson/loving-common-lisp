;;;; cards.lisp — Card, Deck, and Hand Data Structures
;;;;
;;;; Fundamental data types for contract bridge:
;;;; - Cards (suit + rank)
;;;; - Deck (52 cards)
;;;; - Hands (13 cards per player)
;;;; - Shuffle & deal operations

(in-package #:bridge)

;;; ============================================================
;;; SUIT AND RANK CONSTANTS
;;; ============================================================

;; Suits ordered by rank (low to high): Clubs, Diamonds, Hearts, Spades
(defconstant +clubs+    0)
(defconstant +diamonds+ 1)
(defconstant +hearts+   2)
(defconstant +spades+   3)
(defconstant +notrump+  4)  ; Used in bidding, not in cards

(defparameter +suits+ (list +clubs+ +diamonds+ +hearts+ +spades+))

(defparameter +suit-names+
  (vector "Clubs" "Diamonds" "Hearts" "Spades" "No Trump"))

(defparameter +suit-symbols+
  (vector "♣" "♦" "♥" "♠" "NT"))

(defparameter +suit-colors+
  (vector :black :red :red :black))

;; Ranks: 2-14 (14=Ace)
(defparameter +rank-names+
  (let ((table (make-hash-table)))
    (setf (gethash 2 table) "2"
          (gethash 3 table) "3"
          (gethash 4 table) "4"
          (gethash 5 table) "5"
          (gethash 6 table) "6"
          (gethash 7 table) "7"
          (gethash 8 table) "8"
          (gethash 9 table) "9"
          (gethash 10 table) "10"
          (gethash 11 table) "J"
          (gethash 12 table) "Q"
          (gethash 13 table) "K"
          (gethash 14 table) "A")
    table))

(defparameter +ranks+ '(2 3 4 5 6 7 8 9 10 11 12 13 14))

;;; ============================================================
;;; CARD STRUCTURE
;;; ============================================================

(defstruct (card (:constructor make-card (suit rank))
                 (:print-function print-card))
  "A playing card with suit (0-3) and rank (2-14)."
  (suit 0 :type fixnum)
  (rank 2 :type fixnum))

(defun print-card (card stream depth)
  "Custom printer for cards — shows e.g. A♠, K♥, 10♣"
  (declare (ignore depth))
  (format stream "~A~A"
          (gethash (card-rank card) +rank-names+)
          (aref +suit-symbols+ (card-suit card))))

(defun card-name (card)
  "Full name of a card, e.g. 'Ace of Spades'."
  (format nil "~A of ~A"
          (let ((r (card-rank card)))
            (case r
              (14 "Ace") (13 "King") (12 "Queen") (11 "Jack")
              (t (write-to-string r))))
          (aref +suit-names+ (card-suit card))))

(defun card-short-name (card)
  "Short name like 'AS', 'KH', '10C'."
  (format nil "~A~A"
          (gethash (card-rank card) +rank-names+)
          (aref +suit-symbols+ (card-suit card))))

(defun card= (c1 c2)
  "True if two cards are the same."
  (and (= (card-suit c1) (card-suit c2))
       (= (card-rank c1) (card-rank c2))))

(defun card< (c1 c2)
  "Compare cards: by suit first, then rank."
  (or (< (card-suit c1) (card-suit c2))
      (and (= (card-suit c1) (card-suit c2))
           (< (card-rank c1) (card-rank c2)))))

;;; ============================================================
;;; DECK OPERATIONS
;;; ============================================================

(defun make-deck ()
  "Create a standard 52-card deck as a vector."
  (let ((deck (make-array 52 :fill-pointer 0)))
    (dolist (suit +suits+)
      (dolist (rank +ranks+)
        (vector-push (make-card suit rank) deck)))
    deck))

(defun shuffle-deck (deck)
  "Fisher-Yates shuffle — destructively shuffles the deck in place."
  (let ((n (length deck)))
    (loop for i from (1- n) downto 1
          do (let ((j (random (1+ i))))
               (rotatef (aref deck i) (aref deck j))))
    deck))

(defun deal-hands (deck)
  "Deal 13 cards each to 4 players (North, East, South, West).
   Returns an alist: ((0 . hand-n) (1 . hand-e) (2 . hand-s) (3 . hand-w))
   where each hand is a sorted list of cards.
   Players: 0=North, 1=East, 2=South, 3=West."
  (let ((hands (make-array 4 :initial-element nil)))
    (loop for i from 0 below 52
          do (push (aref deck i) (aref hands (mod i 4))))
    ;; Sort each hand by suit (high to low) then rank (high to low)
    (loop for i from 0 below 4
          do (setf (aref hands i)
                   (sort (aref hands i)
                         (lambda (a b)
                           (or (> (card-suit a) (card-suit b))
                               (and (= (card-suit a) (card-suit b))
                                    (> (card-rank a) (card-rank b))))))))
    hands))

;;; ============================================================
;;; PLAYER CONSTANTS
;;; ============================================================

(defconstant +north+ 0)
(defconstant +east+  1)
(defconstant +south+ 2)
(defconstant +west+  3)

(defparameter +player-names+
  (vector "North" "East" "South" "West"))

(defun partner (player)
  "Return the partner of a player."
  (mod (+ player 2) 4))

(defun left-opponent (player)
  "Return the left-hand opponent."
  (mod (+ player 1) 4))

(defun right-opponent (player)
  "Return the right-hand opponent."
  (mod (+ player 3) 4))

(defun next-player (player)
  "Return the next player (clockwise)."
  (mod (+ player 1) 4))

;;; ============================================================
;;; HAND ANALYSIS — HIGH CARD POINTS
;;; ============================================================

(defun card-hcp (card)
  "High card points for a single card (Milton Work count)."
  (let ((rank (card-rank card)))
    (cond ((= rank 14) 4)   ; Ace
          ((= rank 13) 3)   ; King
          ((= rank 12) 2)   ; Queen
          ((= rank 11) 1)   ; Jack
          (t 0))))

(defun hand-hcp (hand)
  "Total high card points in a hand."
  (reduce #'+ hand :key #'card-hcp))

(defun hand-suit-cards (hand suit)
  "Return all cards of a given suit in the hand."
  (remove-if-not (lambda (c) (= (card-suit c) suit)) hand))

(defun hand-suit-length (hand suit)
  "Number of cards in a suit."
  (count suit hand :key #'card-suit))

(defun hand-shape (hand)
  "Return suit distribution as a list (spades hearts diamonds clubs)."
  (list (hand-suit-length hand +spades+)
        (hand-suit-length hand +hearts+)
        (hand-suit-length hand +diamonds+)
        (hand-suit-length hand +clubs+)))

(defun hand-shape-sorted (hand)
  "Return shape sorted descending, e.g. (5 4 3 1)."
  (sort (copy-list (hand-shape hand)) #'>))

;;; ============================================================
;;; HAND ANALYSIS — DISTRIBUTIONAL POINTS
;;; ============================================================

(defun hand-length-points (hand)
  "Length points: +1 per card beyond 4 in any suit."
  (let ((points 0))
    (dolist (suit +suits+)
      (let ((len (hand-suit-length hand suit)))
        (when (> len 4)
          (incf points (- len 4)))))
    points))

(defun hand-shortness-points (hand)
  "Shortness points (used after finding a trump fit):
   void=3, singleton=2, doubleton=1."
  (let ((points 0))
    (dolist (suit +suits+)
      (let ((len (hand-suit-length hand suit)))
        (case len
          (0 (incf points 3))
          (1 (incf points 2))
          (2 (incf points 1)))))
    points))

(defun hand-total-points (hand &optional (trump-fit-found nil))
  "Total points = HCP + distributional points.
   Before trump fit: use length points.
   After trump fit: use shortness points instead."
  (+ (hand-hcp hand)
     (if trump-fit-found
         (hand-shortness-points hand)
         (hand-length-points hand))))

;;; ============================================================
;;; HAND ANALYSIS — SHAPE CLASSIFICATION
;;; ============================================================

(defun hand-balanced-p (hand)
  "A balanced hand has shape 4-3-3-3, 4-4-3-2, or 5-3-3-2."
  (let ((shape (hand-shape-sorted hand)))
    (or (equal shape '(4 3 3 3))
        (equal shape '(4 4 3 2))
        (equal shape '(5 3 3 2)))))

(defun hand-semi-balanced-p (hand)
  "Semi-balanced includes balanced plus 5-4-2-2 and 6-3-2-2."
  (let ((shape (hand-shape-sorted hand)))
    (or (hand-balanced-p hand)
        (equal shape '(5 4 2 2))
        (equal shape '(6 3 2 2)))))

(defun hand-has-major-p (hand &optional (min-length 5))
  "Does the hand have a major suit (hearts/spades) with min-length cards?"
  (or (>= (hand-suit-length hand +hearts+) min-length)
      (>= (hand-suit-length hand +spades+) min-length)))

(defun hand-longest-suit (hand)
  "Return the longest suit. Break ties by higher suit rank."
  (let ((best-suit +spades+)
        (best-len (hand-suit-length hand +spades+)))
    (dolist (suit (list +hearts+ +diamonds+ +clubs+))
      (let ((len (hand-suit-length hand suit)))
        (when (> len best-len)
          (setf best-suit suit
                best-len len))))
    best-suit))

(defun hand-suit-hcp (hand suit)
  "HCP in a specific suit."
  (reduce #'+ (hand-suit-cards hand suit) :key #'card-hcp :initial-value 0))

(defun hand-has-stopper-p (hand suit)
  "Does the hand have a stopper in the given suit?
   A=always, Kx=yes, Qxx=yes, Jxxx=yes."
  (let ((cards (hand-suit-cards hand suit))
        (len (hand-suit-length hand suit)))
    (cond ((= len 0) nil)
          ;; Has an Ace
          ((some (lambda (c) (= (card-rank c) 14)) cards) t)
          ;; Has a King with at least one other card
          ((and (>= len 2)
                (some (lambda (c) (= (card-rank c) 13)) cards)) t)
          ;; Has a Queen with at least two other cards
          ((and (>= len 3)
                (some (lambda (c) (= (card-rank c) 12)) cards)) t)
          ;; Has a Jack with at least three other cards
          ((and (>= len 4)
                (some (lambda (c) (= (card-rank c) 11)) cards)) t)
          (t nil))))

(defun hand-sort (hand)
  "Sort a hand by suit (spades first) then rank (high first)."
  (sort (copy-list hand)
        (lambda (a b)
          (or (> (card-suit a) (card-suit b))
              (and (= (card-suit a) (card-suit b))
                   (> (card-rank a) (card-rank b)))))))
