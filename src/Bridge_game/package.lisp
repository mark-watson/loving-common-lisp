;;;; package.lisp — Bridge Game Package Definition

(defpackage #:bridge
  (:use #:cl)
  (:export
   ;; Card structures
   #:make-card #:card-suit #:card-rank #:card-p
   #:card-name #:card-short-name
   ;; Suit/rank constants
   #:+clubs+ #:+diamonds+ #:+hearts+ #:+spades+
   #:+suits+ #:+suit-names+ #:+suit-symbols+
   #:+rank-names+ #:+ranks+
   ;; Deck operations
   #:make-deck #:shuffle-deck #:deal-hands
   ;; Hand analysis
   #:hand-hcp #:hand-length-points #:hand-distribution-points
   #:hand-total-points #:hand-suit-length #:hand-shape
   #:hand-balanced-p #:hand-sort
   ;; Display
   #:display-card #:display-hand #:display-board
   #:display-hand-by-suit #:display-bidding-history
   ;; Bid structures
   #:make-bid #:bid-level #:bid-suit #:bid-p
   #:bid-name #:bid-pass-p #:bid-double-p #:bid-redouble-p
   #:bid> #:+pass+ #:+double+ #:+redouble+
   ;; Game state
   #:make-game-state #:game-state-hands #:game-state-dealer
   #:game-state-vulnerability #:game-state-bids
   #:game-state-contract #:game-state-declarer
   #:game-state-dummy #:game-state-tricks
   #:game-state-current-trick #:game-state-lead
   #:game-state-phase
   ;; Game engine
   #:new-game #:deal-game #:run-bidding-phase #:run-playing-phase
   #:play-card #:determine-trick-winner
   ;; Rubber bridge scoring
   #:make-rubber-state #:rubber-state-p
   #:score-rubber-deal #:rubber-complete-p
   ;; AI
   #:ai-select-bid #:ai-select-card
   ;; Main
   #:play-bridge #:auto-play))
