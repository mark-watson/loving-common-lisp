# Building a Rubber Bridge AI in Common Lisp

Contract bridge is one of the greatest challenges in game AI. Unlike chess or Go where all information is visible, bridge is a game of *imperfect information* — each player sees only 13 of 52 cards. Players must communicate through a structured bidding language, cooperate with a partner, and compete against opponents who are doing the same. In this chapter we build a complete rubber bridge engine in Common Lisp, featuring rule-based bidding, heuristic card play with Monte Carlo simulation, and full rubber bridge scoring.

## A One-Page Review of How to Play Bridge

Bridge is a trick-taking card game for four players in two partnerships: North-South versus East-West. A standard 52-card deck is dealt evenly — 13 cards per player.

**The Auction.** Starting with the dealer, players bid clockwise. A bid like "2 Hearts" is a contract to win at least 8 tricks (6 + the bid level) with hearts as the trump suit. Each bid must be higher than the previous one (either a higher level, or a higher-ranking suit at the same level). Suits rank: clubs (lowest), diamonds, hearts, spades, no trump (highest). Players may also pass, double an opponent's bid, or redouble. The auction ends after three consecutive passes following at least one suit bid. The final bid becomes the *contract*.

**Declarer, Dummy, and Defenders.** The first player on the winning side to bid the contract's suit becomes the *declarer*. Declarer's partner is the *dummy*, whose hand is laid face-up on the table after the opening lead. Declarer plays both hands. The opposing pair are the *defenders*.

**Trick Play.** The player to declarer's left makes the *opening lead*. Players must follow the suit led if possible; otherwise they may play any card, including a trump. The highest card in the suit led wins the trick — unless a trump was played, in which case the highest trump wins. The trick winner leads next. After all 13 tricks are played, the deal is scored.

**Hand Evaluation.** Players estimate hand strength using *High Card Points* (HCP): Ace=4, King=3, Queen=2, Jack=1. The deck contains 40 total HCP. Distribution adds value: long suits earn length points (+1 per card beyond four), and after finding a trump fit, short suits earn shortness points (void=3, singleton=2, doubleton=1). Game typically requires 25-26 combined points between partners.

**Rubber Bridge Scoring.** A *rubber* is a race to win two *games*. Points scored for tricks bid and made go "below the line" — when they total 100 or more, that side wins a game and becomes *vulnerable* (higher penalties and bonuses). Overtricks, slam bonuses, and penalties go "above the line." The side that wins the rubber earns a bonus of 700 (2-0 sweep) or 500 (2-1). Final scores are the sum of all above-the-line points plus the rubber bonus.

## Design of the Bidding Strategy

Our bidding AI implements a simplified Standard American system — the most widely played bidding convention in North America. The design is entirely rule-based: a decision tree evaluates the hand's high card points, distribution, and the auction context to select a bid.

### Architecture

![Overview of architecture](FIG_bridge.jpg)

The bidding module is organized around three core functions that reflect the natural flow of a bridge auction:

1. **`ai-opening-bid`** — Called when no one has yet made a suit bid. Evaluates HCP and shape to select an opening.
2. **`ai-responding-bid`** — Called when partner has opened but we haven't bid. Applies responder conventions.
3. **`ai-opener-rebid`** — Called when we opened and partner has responded. Handles Stayman, Blackwood, Gerber, and standard rebids.

The top-level entry point `ai-select-bid` dispatches to the appropriate function:

```lisp
(defun ai-select-bid (hand bid-history my-position dealer)
  "Main AI bidding entry point.
   Determines whether to open, respond, rebid, or pass."
  (let* ((current-highest (last-suit-bid bid-history))
         (partner-bid (find-partner-opening bid-history
                                            my-position dealer))
         (my-bid (find-my-opening bid-history my-position dealer))
         (raw-bid
           (cond
             ;; I already bid and partner responded — rebid
             ((and my-bid partner-bid)
              (ai-opener-rebid hand my-bid partner-bid))
             ;; Partner has bid but I haven't — respond
             (partner-bid
              (ai-responding-bid hand partner-bid
                                 bid-history my-position dealer))
             ;; No one has bid a suit yet — try to open
             ((null current-highest)
              (ai-opening-bid hand))
             ;; Opponents have bid, partner hasn't — overcall
             (t
              (let ((hcp (hand-hcp hand)))
                (cond
                  ((and (>= hcp 13) (hand-has-major-p hand 5))
                   (if (>= (hand-suit-length hand +spades+) 5)
                       (make-bid 1 +spades+)
                       (make-bid 1 +hearts+)))
                  ((and (>= hcp 15) (<= hcp 18)
                        (hand-balanced-p hand))
                   (make-bid 1 +notrump+))
                  (t nil)))))))
    (let ((legal-bid (ensure-legal-bid raw-bid current-highest)))
      (or legal-bid (make-pass)))))
```

### Opening Bids

The opening bid decision tree follows Standard American priorities. The strongest hands (22+ HCP) open with the artificial, forcing 2♣. Balanced hands open 1NT (15-17) or 2NT (20-21). Unbalanced hands with 13-21 HCP open at the one level, preferring five-card majors over minors. Weaker hands with long suits open preemptively:

```lisp
(defun ai-opening-bid (hand)
  "Determine AI's opening bid using Standard American conventions."
  (let ((hcp (hand-hcp hand))
        (balanced (hand-balanced-p hand)))
    (cond
      ;; 2♣ — Very strong hand (22+ HCP), artificial forcing
      ((>= hcp 22)
       (make-bid 2 +clubs+))
      ;; 2NT — Balanced, 20-21 HCP
      ((and balanced (>= hcp 20) (<= hcp 21))
       (make-bid 2 +notrump+))
      ;; 1NT — Balanced, 15-17 HCP
      ((and balanced (>= hcp 15) (<= hcp 17))
       (make-bid 1 +notrump+))
      ;; 1 of a suit — 13-21 HCP
      ((and (>= hcp 13) (<= hcp 21))
       (cond
         ;; Both 5-card majors: bid spades (higher-ranking)
         ((and (>= (hand-suit-length hand +spades+) 5)
               (>= (hand-suit-length hand +hearts+) 5))
          (make-bid 1 +spades+))
         ((>= (hand-suit-length hand +spades+) 5)
          (make-bid 1 +spades+))
         ((>= (hand-suit-length hand +hearts+) 5)
          (make-bid 1 +hearts+))
         ;; Minor: longer minor wins
         ((> (hand-suit-length hand +diamonds+)
             (hand-suit-length hand +clubs+))
          (make-bid 1 +diamonds+))
         (t (make-bid 1 +clubs+))))
      ;; Preemptive 3-level — good 7-card suit with 2+ honors
      ;; Weak twos — 6-card suit, sub-opening hand
      ;; ... (see full source for preemptive logic)
      (t nil))))
```

### Responding and Slam Conventions

The responder's logic branches based on partner's opening — 1NT, 2NT, major suit, minor suit, 2♣ strong, weak two, or preemptive. For example, responding to a 1NT opening involves Stayman (bidding 2♣ with 8+ points and a four-card major to ask opener about their major holdings), sign-offs, invitational raises, and slam exploration.

The rebid logic handles the conventional responses to Stayman, Blackwood (4NT asking for aces after suit agreement), and Gerber (4♣ asking for aces after notrump bids):

```lisp
;; Blackwood 4NT: respond with ace count
((blackwood-response-p partner-response)
 (let ((aces (hand-ace-count hand)))
   ;; 5♣=0, 5♦=1, 5♥=2, 5♠=3, 5NT=4
   (make-bid 5 (mod aces 5))))

;; Gerber 4♣ (after NT bids): respond with ace count
((gerber-response-p partner-response my-opening)
 (let ((aces (hand-ace-count hand)))
   (case aces
     (0 (make-bid 4 +diamonds+))
     (1 (make-bid 4 +hearts+))
     (2 (make-bid 4 +spades+))
     (3 (make-bid 4 +notrump+))
     (4 (make-bid 5 +clubs+)))))
```

A safety net function `ensure-legal-bid` guarantees every bid is higher than the current auction level, downgrading illegal bids to a pass.

## Design of the Game Playing Engine

The game engine orchestrates the complete flow of a rubber bridge session: dealing, bidding, trick play, and scoring. It manages both human interaction and AI decision-making.

### Game State

All game information lives in a single `game-state` structure — the central data object threaded through every phase:

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
  (phase          :dealing))
```

### Card and Hand Foundations

Cards are lightweight structs with integer suit (0-3) and rank (2-14). Hands are lists of cards. The hand analysis functions compute everything the bidding and play AI needs:

```lisp
(defstruct (card (:constructor make-card (suit rank)))
  "A playing card with suit (0-3) and rank (2-14)."
  (suit 0 :type fixnum)
  (rank 2 :type fixnum))

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

### Heuristic Card Play AI

The play AI uses positional heuristics drawn from standard bridge technique. When *leading*, the AI selects from a priority list of strategies depending on whether it is declaring or defending:

- **Top of sequence** — Lead the king from K-Q-J (gives partner information)
- **Fourth best** — Lead the fourth-highest card of the longest suit
- **Short suit** — Lead a short side suit (for ruffing potential in trump contracts)

When *following*, the AI applies classical rules: play second-hand low, third-hand high, cover an honor with an honor, and win as cheaply as possible:

```lisp
(defun ai-select-follow (hand trick lead-player lead-suit
                          trump-suit my-position declarer)
  "AI selects a card when following (not leading)."
  (let* ((legal (legal-plays hand lead-suit))
         (my-side (mod my-position 2))
         (decl-side (mod declarer 2))
         (defending (not (= my-side decl-side)))
         (cards-played (length trick))
         (current-winner
           (when (> cards-played 0)
             (trick-winner-partial trick lead-player trump-suit)))
         (partner-winning
           (and current-winner
                (= (mod current-winner 2) my-side))))
    (cond
      ;; Only one legal play
      ((= (length legal) 1) (first legal))
      ;; Must follow suit
      ((some (lambda (c) (= (card-suit c) lead-suit)) legal)
       (let ((in-suit (remove-if-not
                        (lambda (c) (= (card-suit c) lead-suit))
                        legal)))
         (cond
           ;; Partner winning in 4th seat — play low
           ((and (= cards-played 3) partner-winning)
            (lowest-card in-suit))
           ;; Can win — play cheapest winner
           ((can-win-trick in-suit trick lead-player trump-suit)
            (cheapest-winner in-suit trick lead-player trump-suit))
           ;; Can't win — play lowest
           (t (lowest-card in-suit)))))
      ;; Can't follow suit — trump or discard
      (t ...))))
```

### Monte Carlo Simulation

For stronger play, the engine includes a prototype Perfect Information Monte Carlo (PIMC) module. It works by:

1. Generating random deals consistent with known information
2. Trying each legal card and simulating the remaining tricks with heuristic play
3. Selecting the card that produces the best average outcome across samples

```lisp
(defun monte-carlo-select-card (hand trick lead-player lead-suit
                                 trump-suit my-position declarer
                                 ns-tricks ew-tricks cards-played
                                 &key (num-samples 50))
  "Use Monte Carlo simulation to select the best card."
  (let* ((legal (if lead-suit
                    (legal-plays hand lead-suit)
                    (copy-list hand)))
         (remaining (compute-remaining-cards hand cards-played trick))
         (best-card (first legal))
         (best-score most-negative-fixnum))
    ;; Try each legal card
    (dolist (card legal)
      (let ((total-score 0))
        (dotimes (sample num-samples)
          (let* ((sim-hands (generate-consistent-deal ...))
                 (result (simulate-remaining-tricks ...)))
            (if (evenp my-position)
                (incf total-score (car result))
                (incf total-score (cdr result)))))
        (let ((avg-score (/ total-score num-samples)))
          (when (> avg-score best-score)
            (setf best-score avg-score
                  best-card card)))))
    best-card))
```

This approach follows the architecture pioneered by GIB (Ginsberg's Intelligent Bridgeplayer) — the first bridge program to achieve expert-level play.

### The Rubber Game Loop

The top-level `play-bridge` function runs a complete rubber. It creates a `rubber-state` to track games, vulnerability, and cumulative scores, then loops through deals until one side wins two games:

```lisp
(defun play-bridge ()
  "Play a complete rubber of bridge (first to 2 games wins)."
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
                 (setf gs (run-bidding-phase gs))
                 (if (eq (game-state-phase gs) :done)
                     (format t "~%  Deal passed out.~%")
                     (progn
                       (setf gs (run-playing-phase gs))
                       (run-scoring-phase gs rs)))
                 (setf (rubber-state-current-dealer rs)
                       (next-player
                         (rubber-state-current-dealer rs)))))
      (display-rubber-final rs))
    (format t "~%  Thanks for playing!~%")
    rs))
```

A `catch`/`throw` mechanism around `'quit-game` lets the player exit cleanly from any input prompt by typing "Q".

## Design of the Scoring Code

Rubber bridge scoring is notoriously intricate. Points are divided between "below the line" (trick value for tricks bid and made, counting toward game) and "above the line" (everything else). Our scoring module faithfully implements these rules.

### Trick Values

The base value per trick depends on the strain (i.e., the trump suite, or notrump) and whether the contract was doubled:

```lisp
(defun trick-value (suit &optional (doubled 0))
  "Base value per trick for a given strain.
   Clubs/Diamonds = 20, Hearts/Spades = 30, NT = 30."
  (let ((base (cond
                ((<= suit +diamonds+) 20)  ; Minors
                (t 30))))                   ; Majors and NT
    (case doubled
      (1 (* base 2))
      (2 (* base 4))
      (t base))))
```

A contract reaches *game* when the below-the-line trick score totals 100 or more. This means 3NT (40+30+30=100), 4♥/4♠ (4×30=120), or 5♣/5♦ (5×20=100) are the minimum game contracts.

### The Rubber State

The rubber state tracks everything needed across multiple deals:

```lisp
(defstruct rubber-state
  (ns-below      0)    ; N-S points below the line (current game)
  (ew-below      0)    ; E-W points below the line (current game)
  (ns-above      0)    ; N-S points above the line (cumulative)
  (ew-above      0)    ; E-W points above the line (cumulative)
  (ns-games      0)    ; N-S games won in this rubber
  (ew-games      0)    ; E-W games won in this rubber
  (ns-vulnerable nil)
  (ew-vulnerable nil)
  (current-dealer +north+)
  (deals-played   0))
```

### Scoring a Deal

The `score-rubber-deal` function handles both made and defeated contracts. When a contract is made, the trick score goes below the line and bonuses (overtricks, insult, slam) go above. When defeated, penalties go above the line *for the defenders*. Vulnerability affects penalty and bonus amounts significantly:

```lisp
;; Contract made: record below-the-line and above-the-line points
(if made
    (let ((below-score (contract-trick-score level suit doubled))
          (above-score 0))
      ;; Overtrick bonus (above the line)
      (when (> overtricks 0)
        (incf above-score
              (case doubled
                (0 (* overtricks (trick-value suit)))
                (1 (* overtricks (if vul 200 100)))
                (2 (* overtricks (if vul 400 200))))))
      ;; Insult bonus for doubled/redoubled contracts
      (case doubled
        (1 (incf above-score 50))
        (2 (incf above-score 100)))
      ;; Slam bonuses
      (when (slam-p level)
        (incf above-score (if vul 750 500)))
      (when (grand-slam-p level)
        (incf above-score (if vul 1500 1000)))
      ;; Record scores and check for game
      ...)

    ;; Contract defeated: penalties to defenders
    (let* ((down (abs overtricks))
           (penalty
             (case doubled
               (0 (* down (if vul 100 50)))
               (1 (if vul
                      (+ 200 (* (max 0 (1- down)) 300))
                      (cond
                        ((= down 1) 100)
                        ((= down 2) 300)
                        ((= down 3) 500)
                        (t (+ 500 (* (- down 3) 300))))))
               (2 (* 2 (undoubled-penalty ...))))))
      ...))
```

When a side's below-the-line total reaches 100, they win a game, become vulnerable, and *both* sides' below-the-line scores reset to zero. The rubber is complete when one side wins two games.

## Design of the UI

The UI is text-based, designed for terminal play. It renders the bridge table as a compass-rose layout showing all four positions, with hidden hands displayed as card counts and visible hands (the human player and dummy) shown suit-by-suit.

### Board Display

The `display-board` function renders the classic bridge table layout:

```lisp
(defun display-board (hands &key (human-player +south+)
                                  (dummy nil)
                                  (current-trick nil)
                                  (declarer nil)
                                  (vulnerability nil)
                                  (stream t))
  "Display the bridge table with all four hands.
   Shows human player's hand fully; others hidden unless dummy."
  (let* ((show-north (or (= human-player +north+)
                         (eql dummy +north+)))
         (show-south (or (= human-player +south+)
                         (eql dummy +south+)))
         ...)
    ;; Header with suit symbols
    ;; North hand (centered, top)
    ;; West and East hands (side by side, middle row)
    ;; Current trick display (center)
    ;; South hand (centered, bottom)
    ...))
```

A typical board display looks like this:

```
════════════════════════════════════════════════════════════
          ♠ ♥ ♦ ♣  BRIDGE TABLE  ♣ ♦ ♥ ♠
                    Vul: None
────────────────────────────────────────────────────────────
                         NORTH
                     ♠ A K 10 3
                     ♥ Q 7
                     ♦ J 9 5 2
                     ♣ K 8 4

WEST                                    EAST
[13 cards]                              [13 cards]

                      SOUTH (You)
                     ♠ Q J 8
                     ♥ A K 10 9 5
                     ♦ A 3
                     ♣ Q 7 2
════════════════════════════════════════════════════════════
```

### Bidding and Scoring Displays

The bidding history is rendered as a four-column table aligned to the dealer position. The rubber scorecard uses box-drawing characters for a clean, traditional look:

```
╔════════════════════╦════════════════════╗
║   RUBBER BRIDGE SCORECARD               ║
╠════════════════════╬════════════════════╣
║      N-S           ║      E-W           ║
╠════════════════════╬════════════════════╣
║  Above:   250      ║  Above:   100      ║
╠────────────────────╬────────────────────╣
║  Below:    60      ║  Below:     0      ║
║  Games: 1          ║  Games: 0          ║
║  Vul:   YES        ║  Vul:   No         ║
╚════════════════════╩════════════════════╝
```

### Human Input Parsing

The engine accepts both natural card names and numeric indices. Bid input supports common abbreviations:

```lisp
(defun parse-bid-input (input)
  "Parse a bid string like '1H', '3NT', 'PASS', 'DBL'."
  (let ((s (string-upcase (string-trim " " input))))
    (cond
      ((or (string= s "P") (string= s "PASS"))
       (make-pass))
      ((or (string= s "D") (string= s "DBL")
           (string= s "DOUBLE") (string= s "X"))
       (make-double-bid))
      ((or (string= s "R") (string= s "RDBL")
           (string= s "REDOUBLE") (string= s "XX"))
       (make-redouble-bid))
      (t
       ;; Parse level + strain, e.g. "2H" → (make-bid 2 +hearts+)
       ...))))
```

Card input is similarly flexible — `AS` for Ace of Spades, `10C` for Ten of Clubs, or just `3` to select the third legal play by index number.

### System Definition

The ASDF system definition ties all modules together in dependency order:

```lisp
(asdf:defsystem #:bridge
  :description "Contract Bridge AI — text-based bridge game
                with AI opponents"
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

Loading and playing is straightforward from the REPL:

```lisp
(asdf:load-system :bridge)
(bridge:play-bridge)     ;; Interactive rubber (you play South)
(bridge:auto-play)       ;; All-AI game for testing
```

## Wrap Up

Building a bridge AI brings together several fascinating areas of computer science: game tree search under imperfect information, rule-based expert systems, Monte Carlo methods, and interactive system design.

Our implementation takes a pragmatic, layered approach. The bidding module encodes Standard American conventions as a decision tree — this is effective because bidding conventions are essentially a formalized protocol with well-defined rules. The play module combines fast heuristic strategies (top of sequence, fourth best, second-hand low) with optional Monte Carlo sampling for stronger decisions. The scoring module faithfully tracks the intricate rubber bridge accounting of below/above the line, game, vulnerability, and rubber bonuses.

Common Lisp proves to be an excellent language for this domain. Structs give us efficient data representation for the millions of cards and tricks processed during Monte Carlo simulation. The REPL enables rapid testing of individual functions — we can evaluate a hand's HCP, test a bidding sequence, or simulate a trick in isolation. The `catch`/`throw` mechanism provides clean control flow for the interactive game loop.

Areas for future improvement include:

- **Competitive bidding** — Our overcall logic is minimal. A full implementation would include takeout doubles, Michaels cue bids, and unusual 2NT.
- **Defensive signaling** — The AI doesn't yet signal (attitude, count, or suit preference) to its partner during defense.
- **Double dummy solver** — Replacing Monte Carlo's heuristic playouts with a proper alpha-beta DDS with partition search would dramatically improve play accuracy.
- **Neural evaluation** — Modern bridge AIs like NuKKAI combine symbolic rules with neural networks trained on millions of expert-played deals. A neural component could improve both bidding judgment and play in ambiguous situations.

The complete source code for this project is available at the book's [GitHub repository in the Bridge_game directory](https://github.com/mark-watson/loving-common-lisp/tree/main/src/Bridge_game).
