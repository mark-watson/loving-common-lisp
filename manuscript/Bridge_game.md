# A Rubber Bridge Game with AI Players

Contract bridge is a hard game for AI. In chess or Go both players see the whole board. In bridge each player sees only 13 of the 52 cards. Partners must share what they hold through a small, formal bidding language, and they must work together against two opponents who do the same. In this chapter we build a complete rubber bridge program in Common Lisp. It has rule-based bidding, heuristic card play, a prototype Monte Carlo player, and full rubber bridge scoring. You play South in a text UI and the program plays the other three seats.

The example code is in the directory **src/Bridge_game**.

## How to Play Bridge in One Page

Four players form two teams: North-South and East-West. The dealer deals the 52-card deck evenly, so each player holds 13 cards.

**The auction.** Starting with the dealer, players bid in turn, clockwise. A bid such as "2 Hearts" promises to win at least `6 + \text{level}`$ tricks, here 8, with hearts as the trump suit. Each bid must top the last one, either at a higher level or with a higher-ranking suit at the same level. Suits rank from low to high: clubs, diamonds, hearts, spades, and then no trump. A player may also pass, double an opponent's bid, or redouble an opponent's double. The auction ends after three passes in a row that follow at least one suit bid. The last bid becomes the *contract*. A double or redouble applies to the contract only if it comes after that bid. A later bid cancels a double of an earlier bid.

**Declarer, dummy, and defenders.** On the side that won the auction, the first player to bid the contract's suit becomes the *declarer*. Declarer's partner is the *dummy*. After the opening lead, dummy lays their hand face up and declarer plays both hands. The other team are the *defenders*.

**Trick play.** The player to declarer's left makes the *opening lead*. Each player must follow the suit led if they can. If they can not, they may play any card, including a trump. The highest card of the suit led wins the trick, unless someone played a trump, in which case the highest trump wins. The winner of each trick leads to the next one. After all 13 tricks, we score the deal.

**Hand evaluation.** Players judge a hand with *high card points* (HCP): Ace 4, King 3, Queen 2, Jack 1. The deck holds 40 HCP. Shape adds value. Before partners find a trump fit, each card past four in a suit adds one *length point*. After they find a fit, short suits add *shortness points* instead: void 3, singleton 2, doubleton 1. A game contract usually needs 25 or 26 points between the two hands.

**Rubber bridge scoring.** A *rubber* is a race to win two *games*. Points for tricks bid and made go "below the line." When a side's below-the-line total reaches 100, that side wins a game and becomes *vulnerable*, which raises its bonuses and its penalties. Overtricks, slam bonuses, and penalties go "above the line." The side that wins the rubber earns a bonus of 700 for a 2-0 win or 500 for a 2-1 win. Each side's final score adds all its points above and below the line, plus any rubber bonus.

## Running the Program

The ASDF system lists the source files in load order:

```lisp
(asdf:defsystem #:bridge
  :description "Contract Bridge AI — text-based bridge game with AI opponents"
  :author "Mark Watson"
  :license "Apache-2.0"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "cards")
               (:file "bidding")
               (:file "display")
               (:file "play")
               (:file "scoring")
               (:file "engine")))
```

Load the system and start a game from the REPL:

```lisp
(asdf:load-system :bridge)
(bridge:play-bridge)   ; play a full rubber as South
(bridge:auto-play)     ; one deal with four AI players
```

At a bid prompt you type `1H`, `3NT`, `PASS` (or `P`), `DBL` (or `X`), or `RDBL` (or `XX`). At a card prompt you type a card name such as `AS`, `KH`, or `10C`, or the number of a card in the list of legal plays. Type `Q` at any prompt to quit.

Here is the output of one call to **auto-play**. The program first prints all four hands with their HCP and shape (spades, hearts, diamonds, clubs), then the auction, then each trick in play order with the trick's winner:

```text
═══ AUTO-PLAY GAME ═══

All hands:

North:
  ♠: A Q 6 3
  ♥: A 4
  ♦: K J 7 5
  ♣: Q 9 5
  HCP: 16  Shape: 4-2-4-3-  Total: 16

East:
  ♠: K 2
  ♥: 5 3
  ♦: A 10 9 6
  ♣: J 10 8 7 3
  HCP: 8  Shape: 2-2-4-5-  Total: 9

South:
  ♠: J 9 8 5 4
  ♥: K 10 8
  ♦: Q
  ♣: A K 6 4
  HCP: 13  Shape: 5-3-1-4-  Total: 14

West:
  ♠: 10 7
  ♥: Q J 9 7 6 2
  ♦: 8 4 3 2
  ♣: 2
  HCP: 3  Shape: 2-6-4-1-  Total: 5
  North: 1NT
  East: Pass
  South: 2♣
  West: Pass
  North: 2♠
  East: Pass
  South: Pass
  West: Pass

  Contract: 2 Spades by North
  Trick  1: 7♣ K♣ 2♣ 5♣ → South
  Trick  2: J♠ 7♠ Q♠ K♠ → East
  Trick  3: 3♣ 4♣ 10♠ 9♣ → West
  Trick  4: 7♥ A♥ 3♥ 8♥ → North
  Trick  5: A♠ 2♠ 4♠ 2♦ → North
  Trick  6: 5♦ 6♦ Q♦ 3♦ → South
  Trick  7: 9♠ 2♥ 3♠ 5♥ → South
  Trick  8: 8♠ 4♦ 6♠ 8♣ → South
  Trick  9: 5♠ 6♥ 4♥ 9♦ → South
  Trick 10: K♥ 9♥ 7♦ 10♣ → South
  Trick 11: 10♥ J♥ J♦ 10♦ → West
  Trick 12: 8♦ K♦ A♦ 6♣ → East
  Trick 13: J♣ A♣ Q♥ Q♣ → South

  N-S: 9 tricks, E-W: 4 tricks
  Score: 60
```

North holds a balanced 16 HCP and opens 1NT. South has 13 HCP and a five-card spade suit, so South bids 2♣, the Stayman convention, to ask North for a four-card major. North shows four spades with 2♠. North-South make nine tricks, one more than the eight they need. The 60 points for two spade tricks go below the line. We return to this auction later, because South should have bid on to game.

## Architecture

The code has six layers. Each file depends only on files that load before it:

{width: "80%"}
![Architecture of the rubber bridge program](images/bridge_architecture.jpg)

| File | Role |
|---|---|
| `package.lisp` | Defines the `bridge` package and its exports |
| `cards.lisp` | Cards, the deck, shuffle and deal, hand analysis |
| `bidding.lisp` | Bids, auction rules, and the bidding AI |
| `display.lisp` | Text rendering of hands, the table, and the auction |
| `play.lisp` | Legal plays, trick winners, the card play AI, Monte Carlo play |
| `scoring.lisp` | Rubber bridge scoring and the scorecard |
| `engine.lisp` | Game state, human input, the bidding, play, and scoring phases |

## Cards and Hands

A card is a small struct with an integer suit from 0 to 3 and an integer rank from 2 to 14, where 14 is the Ace. Constants name the suits, and `+notrump+` (4) extends the suit range for bids:

```lisp
(defconstant +clubs+    0)
(defconstant +diamonds+ 1)
(defconstant +hearts+   2)
(defconstant +spades+   3)
(defconstant +notrump+  4)  ; Used in bidding, not in cards

(defstruct (card (:constructor make-card (suit rank))
                 (:print-function print-card))
  "A playing card with suit (0-3) and rank (2-14)."
  (suit 0 :type fixnum)
  (rank 2 :type fixnum))
```

Since suits are ordered integers, the code compares bids and finds trumps with plain arithmetic. Players are also integers: North 0, East 1, South 2, West 3. So `(mod player 2)` gives the team (0 for North-South) and `(mod (+ player 2) 4)` gives the partner.

**make-deck** builds a 52-card vector and **shuffle-deck** runs a Fisher-Yates shuffle on it in place. A hand is a list of cards. The hand analysis functions compute what the bidding AI needs:

```lisp
(defun hand-hcp (hand)
  "Total high card points in a hand."
  (reduce #'+ hand :key #'card-hcp))

(defun hand-total-points (hand &optional (trump-fit-found nil))
  "Total points = HCP + distributional points.
   Before trump fit: use length points.
   After trump fit: use shortness points instead."
  (+ (hand-hcp hand)
     (if trump-fit-found
         (hand-shortness-points hand)
         (hand-length-points hand))))

(defun hand-balanced-p (hand)
  "A balanced hand has shape 4-3-3-3, 4-4-3-2, or 5-3-3-2."
  (let ((shape (hand-shape-sorted hand)))
    (or (equal shape '(4 3 3 3))
        (equal shape '(4 4 3 2))
        (equal shape '(5 3 3 2)))))
```

Other helpers in `cards.lisp` find suit lengths, the longest suit, HCP in one suit, and stoppers (a card that will stop the opponents from running a suit in a no trump contract).

## The Bidding AI

The bidding AI uses a simplified form of Standard American, the most common bidding system in North America. It is a rule-based decision tree. Each rule tests the hand's HCP, its shape, and the bids made so far. The file `BRIDGE_BIDDING_DECISIONS.md` lists the rules in plain English, and the docstrings in `bidding.lisp` point back to it.

### Bids and the Auction

A bid is a struct with a type (`:suit`, `:pass`, `:double`, or `:redouble`), a level, and a suit. To compare two suit bids, **bid-index** maps them onto one scale, from 1♣ at 0 up to 7NT at 34:

```lisp
(defun bid-index (bid)
  "Numeric index for comparing bids. 1♣=0, 1♦=1, ..., 7NT=34."
  (when (bid-suit-p bid)
    (+ (* (1- (bid-level bid)) 5)
       (bid-suit bid))))
```

The auction history is a list with the newest bid first. To find who made a given bid, the code counts back from the dealer: the bid at index `i` came from player `(mod (+ dealer (- (length bid-history) 1 i)) 4)`.

**extract-contract** turns a finished auction into a contract. It walks the history from newest to oldest until it reaches the last suit bid, and it records any double or redouble it passes on the way. A double that comes before the last suit bid does not count, because the later bid cancelled it. Then it walks from oldest to newest to find the first player on the declaring side who bid the contract's suit. That player becomes declarer:

```lisp
(defun extract-contract (bid-history dealer)
  "Extract the final contract from the bidding.
   Returns (values contract-bid declarer doubled-state).
   doubled-state: 0=undoubled, 1=doubled, 2=redoubled."
  (let ((final-bid nil)
        (final-bidder nil)
        (doubled 0))
    (loop for i from 0 below (length bid-history)
          for bid = (nth i bid-history)
          do (cond
               ((bid-suit-p bid)
                (setf final-bid bid
                      final-bidder (mod (+ dealer (- (length bid-history) 1 i)) 4))
                (return))
               ((bid-redouble-p bid)
                (setf doubled 2))
               ((bid-double-p bid)
                (setf doubled (max doubled 1)))))
    (when final-bid
      (let* ((declaring-side (mod final-bidder 2))
             (contract-suit (bid-suit final-bid))
             (declarer final-bidder))
        (loop for i from (1- (length bid-history)) downto 0
              for bid = (nth i bid-history)
              for bidder = (mod (+ dealer (- (length bid-history) 1 i)) 4)
              do (when (and (bid-suit-p bid)
                            (= (bid-suit bid) contract-suit)
                            (= (mod bidder 2) declaring-side))
                   (setf declarer bidder)
                   (return)))
        (values final-bid declarer doubled)))))
```

In the sample deal North bid spades first, so North became declarer even though South made the Stayman bid.

### The Top-Level Bidding Function

**ai-select-bid** decides which of three cases applies and calls the matching function:

1. **ai-opening-bid** when no one has made a suit bid yet.
2. **ai-responding-bid** when partner has bid and we have not.
3. **ai-opener-rebid** when both we and partner have bid.

If only the opponents have bid, the AI makes a simple overcall: one of a five-card major with 13+ HCP, or 1NT with a balanced 15 to 18 HCP:

```lisp
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
```

The rule functions return `nil` to mean pass. They also do not check that a bid tops the current one. **ensure-legal-bid** does that last check and turns any bid that is too low into a pass. This keeps each rule short.

### Opening Bids

**ai-opening-bid** follows the Standard American order. With 22+ HCP it opens 2♣, an artificial bid that forces partner to reply. A balanced hand opens 2NT with 20 or 21 HCP and 1NT with 15 to 17. Other hands with 13 to 21 HCP open one of a suit, with a five-card major ahead of a minor. Below 13 HCP, a seven-card suit with two honors opens at the three level, and a six-card suit other than clubs opens a weak two:

```lisp
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
```

### Responses

**ai-responding-bid** branches on partner's opening: 1NT, 2NT, one of a major, one of a minor, 2♣, a weak two, or a three-level preempt. Each branch then picks a bid by HCP and suit lengths. For example, over partner's 1NT the AI:

- passes, or signs off in a five-card suit at the two level, with 0 to 7 HCP;
- bids 2♣ (Stayman) with 8+ HCP and a four-card major;
- bids 2NT, inviting game, with 8 or 9 HCP;
- with 10 to 15 HCP, bids game in a six-card major, bids three of a five-card major to invite, or else bids 3NT;
- bids 4NT, a slam try, with 16+ HCP.

Over a one-level minor opening the AI looks first for a four-card major, then for a no trump bid, and last for a raise of partner's minor.

### Rebids and Slam Conventions

**ai-opener-rebid** first checks for three conventions that ask a question:

- **Stayman** (2♣ over 1NT, or 3♣ over 2NT) asks for a four-card major. Opener bids hearts if they hold four, then spades, and else diamonds to deny a major.
- **Blackwood** (4NT after a suit bid) asks for aces. **Blackwood** 5NT then asks for kings.
- **Gerber** (4♣ after a no trump bid) asks for aces, and 5♣ then asks for kings.

The Blackwood reply needs no table. The reply suits 5♣, 5♦, 5♥, 5♠, 5NT stand for 0 to 4 aces, and the suit integers run from 0 to 4 in that same order, so the ace count gives the suit. Since a hand holds at most four aces, `(mod aces 5)` just returns `aces`:

```lisp
      ;; ── Blackwood 4NT: respond with ace count ──
      ((blackwood-response-p partner-response)
       (let ((aces (hand-ace-count hand)))
         ;; 5♣=0, 5♦=1, 5♥=2, 5♠=3, 5NT=4
         (make-bid 5 (mod aces 5))))
```

Gerber replies start at 4♦ and run past 4NT to 5♣, so that branch spells out each case:

```lisp
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
```

If none of the conventions apply, opener raises partner's no trump with extra values, bids game or tries for slam after partner raises, or supports partner's new suit.

### A Weakness in the Sample Auction

The sample deal shows a limit of this design. **find-partner-opening** and **find-my-opening** each return a player's *first* suit bid. When South must bid after North's 2♠, **ai-select-bid** sees South's 2♣ as "my bid" and North's 1NT as "partner's bid," so it calls **ai-opener-rebid** as if South had opened 2♣ and North had replied 1NT. The no trump branch passes with 15 HCP or less. South never sees North's 2♠, so it passes with a nine-card spade fit and 29 combined HCP, enough to bid a 4♠ game. A better design would pass the whole auction to the rebid logic. That is a good exercise for the reader.

## The Card Play AI

### Rules of Play

**legal-plays** returns every card in hand when leading, the cards in the suit led when the player can follow, and every card when they can not. **trick-winner** takes the four cards of a trick in play order and the index of the player who led:

```lisp
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
```

**trick-winner-partial** does the same for a trick with fewer than four cards. The AI uses it to find out who is winning the trick so far.

### Leading

**ai-select-card** calls **ai-select-lead** when the trick is empty and **ai-select-follow** otherwise. A defender tries a list of standard leads and takes the first that applies:

1. **Top of a sequence:** lead the King from K-Q-J, which tells partner you hold the Queen.
2. **Fourth best:** lead the fourth-highest card of the longest side suit.
3. **Short suit:** in a trump contract, lead the top card of the shortest side suit, hoping to ruff that suit later.
4. Else lead the lowest card of the longest side suit.

Declarer draws trumps first if they hold three or more with the Queen or better, then tries a sequence lead and then fourth best. In the sample deal East led the 7♣ from J-10-8-7-3. The J-10-8 is not a sequence, so the AI chose the fourth-best card.

### Following

When following, the AI applies a few classic rules: play low when partner is winning in fourth seat, win as cheaply as possible, and play low when it can not win:

```lisp
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
       (let* ((trumps (when trump-suit
                        (remove-if-not (lambda (c) (= (card-suit c) trump-suit)) legal)))
              (non-trumps (if trump-suit
                              (remove-if (lambda (c) (= (card-suit c) trump-suit)) legal)
                              legal))
              ;; Only trumps that beat the current winner (which may itself
              ;; be a trump) actually win the trick
              (winner (when trick (current-winning-card trick lead-player trump-suit)))
              (winning-trumps
                (remove-if-not (lambda (c)
                                 (or (null winner)
                                     (/= (card-suit winner) trump-suit)
                                     (> (card-rank c) (card-rank winner))))
                               trumps)))
         (cond
           ;; Defending: trump if possible and partner isn't winning
           ((and defending winning-trumps (not partner-winning))
            (lowest-card winning-trumps)) ; Use lowest trump that wins
           ;; Declaring: trump when beneficial
           ((and (not defending) winning-trumps (not partner-winning))
            (lowest-card winning-trumps))
           ;; Discard — throw away lowest card from weakest suit
           ;; (or the lowest trump when holding nothing but trumps)
           (t (or (best-discard non-trumps trump-suit)
                  (lowest-card trumps)))))))))
```

A card that follows suit can win only if it outranks the card now winning, and only if that card is in the same suit. Once someone ruffs, no card in the suit led can win, however high. **beats-current-winner-p** makes this test, and both **can-win-trick** and **cheapest-winner** call it:

```lisp
(defun current-winning-card (trick lead-player trump-suit)
  "The card currently winning the partial TRICK."
  (nth (mod (- (trick-winner-partial trick lead-player trump-suit) lead-player) 4)
       trick))

(defun beats-current-winner-p (card trick lead-player trump-suit)
  "Would CARD, which follows the lead suit, beat the current winner of TRICK?
   It can only win if the trick has not been trumped (or the lead suit is
   trump) and it outranks the winning card."
  (let ((winner (current-winning-card trick lead-player trump-suit)))
    (and (= (card-suit card) (card-suit winner))
         (> (card-rank card) (card-rank winner)))))
```

One detail needs care. The game engine builds each trick with `push`, so its list holds the newest card first. The play AI expects play order, with the lead card first, so the engine passes `(reverse trick)` to **ai-select-card**. With the wrong order, **trick-winner-partial** would treat the last card played as the lead.

### Monte Carlo Card Play

`play.lisp` also holds a prototype of Perfect Information Monte Carlo (PIMC) play. The AI can not see the other hands, so it guesses them many times and averages the results:

1. Deal the unseen cards at random to the other three players, giving each the right number of cards.
2. For each legal card, play it and then play out the rest of the deal with the heuristic AI in all four seats.
3. Choose the card with the highest average number of tricks for our side.

```lisp
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
    ...
    (dolist (card legal)
      (let ((total-score 0))
        (dotimes (sample num-samples)
          (let* ((sim-hands (generate-consistent-deal
                             (remove card hand :test #'card= :count 1)
                             my-position remaining other-sizes))
                 ...)
            ...
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
```

The cost grows with the number of legal cards times `num-samples` times the number of tricks left. The random deals use only the cards seen so far. They ignore what the auction revealed, such as a 1NT opener's balanced shape. The game loop does not call **monte-carlo-select-card** yet; **ai-select-card** uses only the heuristics. You can call it from the REPL to compare its choices with the heuristic player. Matthew Ginsberg's GIB program used this PIMC approach, with a fast double dummy solver in place of heuristic playouts, and became the first bridge program to play at expert level.

## Scoring

Rubber bridge scoring has many special cases. `scoring.lisp` keeps the state of a rubber in one struct:

```lisp
(defstruct rubber-state
  "State of an ongoing rubber bridge match."
  ;; Below the line: trick points toward game
  (ns-below      0)    ; N-S points below the line (current game)
  (ew-below      0)    ; E-W points below the line (current game)
  ;; Below-the-line points over the whole rubber; NS-BELOW / EW-BELOW are
  ;; reset after each game but every trick point counts in the final total
  (ns-below-total 0)
  (ew-below-total 0)
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
```

### Trick Values

Each trick bid and made in clubs or diamonds is worth 20 points, and each in hearts, spades, or no trump is worth 30. The first no trump trick earns 10 more. A double multiplies trick values by 2 and a redouble by 4:

```lisp
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
```

**contract-trick-score** adds the extra no trump points. A side wins a game when its trick score reaches 100, so the lowest undoubled game contracts are 3NT (`40 + 30 + 30 = 100`$), 4♥ or 4♠ (`4 \times 30 = 120`$), and 5♣ or 5♦ (`5 \times 20 = 100`$).

### Scoring a Deal

**score-rubber-deal** takes the contract, the tricks declarer won, and the rubber state, and updates the state in place. When declarer makes the contract, the trick score goes below the line, and overtricks, the "insult" bonus for making a doubled contract, and slam bonuses go above it. A grand slam earns its own bonus in place of the small slam bonus, so the code uses one `cond`. Two `when` forms would award both, since **slam-p** is true at levels 6 and 7:

```lisp
          ;; Slam bonus (above the line): a grand slam earns the grand slam
          ;; bonus instead of (not in addition to) the small slam bonus
          (cond ((grand-slam-p level)
                 (incf above-score (if vul 1500 1000)))
                ((slam-p level)
                 (incf above-score (if vul 750 500))))
```

When a side reaches 100 below the line, it wins a game and becomes vulnerable, and *both* sides' below-the-line scores for the current game reset to zero. The slots `ns-below-total` and `ew-below-total` keep running totals over the whole rubber, because the final score counts every trick point, not only those in the current game.

When declarer fails, the defenders score a penalty above the line for each trick short ("down"). Undoubled, each down trick costs 50, or 100 when vulnerable. Doubled penalties grow faster. Not vulnerable they run 100, 300, 500, and then 300 more for each extra trick. Vulnerable they run 200 and then 300 more for each extra trick. Redoubled penalties are twice the doubled ones:

```lisp
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

          (- penalty))
```

The function returns the below-the-line score for a made contract and the penalty as a negative number for a failed one.

Here is a short REPL test. South makes 4♥ with one overtrick, which wins a game. Then East goes down one in 2♠:

```lisp
(in-package :bridge)
(let ((rs (make-rubber-state)))
  (score-rubber-deal 4 +hearts+ 11 +south+ 0 rs)
  (score-rubber-deal 2 +spades+ 7 +east+ 0 rs)
  (display-rubber-score rs)
  (multiple-value-list (rubber-total-scores rs)))
```

The scorecard shows North-South with one game, vulnerable, and 80 points above the line: 30 for the overtrick and 50 for East's down trick. Their 120 points below the line moved to the rubber total when they won the game:

```text
╔════════════════════╦════════════════════╗
║   RUBBER BRIDGE SCORECARD              ║
╠════════════════════╬════════════════════╣
║      N-S           ║      E-W           ║
╠════════════════════╬════════════════════╣
║  Above:    80       ║  Above:     0       ║
╠────────────────────╬────────────────────╣
║  Below:     0       ║  Below:     0       ║
║  Games: 1         ║  Games: 0         ║
║  Vul:   YES       ║  Vul:   No        ║
╚════════════════════╩════════════════════╝
```

**rubber-total-scores** returns `(200 0)`: 80 above the line plus 120 below for North-South. No one has won the rubber yet, so there is no rubber bonus.

## The Game Engine

### Game State

`engine.lisp` keeps everything about one deal in a single struct that each phase reads and updates:

```lisp
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
```

### The Three Phases

**run-bidding-phase** asks each seat for a bid in turn: the human at South through **prompt-for-bid**, and the AI elsewhere through **ai-select-bid**. It stops after three passes that follow a suit bid, or four passes if no one opened. Then it calls **extract-contract** and sets the declarer, dummy, trump suit, and opening leader.

The human always sits South. If North wins the contract, South would be dummy and have nothing to do. To avoid that, the engine swaps the North and South hands, so the human plays declarer from the South seat and North holds the dummy cards:

```lisp
          (when (and (= (game-state-human-player gs) +south+)
                     (= declarer +north+))
            (let ((north-cards (aref (game-state-hands gs) +north+))
                  (south-cards (aref (game-state-hands gs) +south+)))
              (setf (aref (game-state-hands gs) +south+) north-cards
                    (aref (game-state-hands gs) +north+) south-cards))
            ;; Swap dummy designation: North now holds the dummy cards
            (setf (game-state-dummy gs) +north+)
            ...)
```

**run-playing-phase** plays 13 tricks. For each card it decides whether the human or the AI plays it. The human plays their own hand and also plays dummy when North-South declare. **run-scoring-phase** finds the tricks declarer's side won, calls **score-rubber-deal**, and prints the deal result and the scorecard.

### The Rubber Loop

**play-bridge** runs a whole rubber. It deals, bids, plays, and scores deal after deal, passing the dealer clockwise, until one side wins two games:

```lisp
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
```

The input prompts sit deep inside the bidding and play loops. When the user types `Q`, the prompt calls `(throw 'quit-game nil)`, and control jumps straight to the `catch` in **play-bridge**. No function in between needs to check for a quit flag.

`scoring.lisp` runs `(setf *random-state* (make-random-state t))` at load time, so each session deals new hands. To repeat a deal while testing, set `*random-state*` to a fixed seed before you call **auto-play**.

## The Text UI

`display.lisp` draws the table as a compass, with North on top, West and East in the middle, and South at the bottom. It shows the human's hand and the dummy in full, and shows only a card count for the other two hands. This is the board for the sample deal, from South's seat with North as dummy:

```text
════════════════════════════════════════════════════════════
               ♠ ♥ ♦ ♣  BRIDGE TABLE  ♣ ♦ ♥ ♠
                         Vul: NONE
────────────────────────────────────────────────────────────
                           NORTH
                         ♠ A Q 6 3
                           ♥ A 4
                         ♦ K J 7 5
                          ♣ Q 9 5

WEST                                    EAST
[13 cards]                              [13 cards]

                        SOUTH (You)
                        ♠ J 9 8 5 4
                          ♥ K 10 8
                            ♦ Q
                         ♣ A K 6 4
════════════════════════════════════════════════════════════
```

**display-bidding-history** prints the auction in four columns that line up under the dealer's seat. The card prompt lists the legal plays with numbers, so you can type `2` in place of `KH`.

The input parsers accept several spellings. **parse-bid-input** maps `P` and `PASS` to a pass, `D`, `DBL`, `DOUBLE`, and `X` to a double, and `R`, `RDBL`, `REDOUBLE`, and `XX` to a redouble. Any other input must be a level from 1 to 7 followed by a strain such as `H`, `HEARTS`, `♥`, `N`, or `NT`. **parse-card-input** first reads an all-digit string as an index into the legal plays, and else reads a rank followed by a suit letter.

## Wrap Up

This program combines several AI techniques. The bidding module encodes Standard American as a decision tree. That works well because a bidding system is a formal protocol with written rules. The play module uses fast heuristics from bridge books, and it includes a Monte Carlo player that samples the hidden hands. The scoring module tracks the rubber bridge rules for the line, games, vulnerability, and bonuses.

Common Lisp suits this kind of program. Small structs with typed integer slots keep cards and bids cheap to create and compare, which matters when Monte Carlo play deals thousands of sample hands. The REPL lets you test one piece at a time: count a hand's HCP, run one auction, or score one deal. `catch` and `throw` give the game loop a clean exit from deep inside the input code.

Here are some ways to extend the program:

- **Pass the full auction to the rebid logic.** As the sample deal shows, the rebid code sees only each player's first bid.
- **Competitive bidding.** The overcall logic is minimal. A fuller version would add takeout doubles, Michaels cue bids, and the unusual 2NT.
- **Defensive signals.** The AI does not yet signal attitude, count, or suit preference to its partner.
- **Use Monte Carlo play in the game.** Call **monte-carlo-select-card** from **ai-select-card** for declarer, and bias the sample deals with what the auction revealed.
- **A double dummy solver.** Replacing the heuristic playouts with an alpha-beta search over the visible sample hands would make each Monte Carlo sample far more accurate.
