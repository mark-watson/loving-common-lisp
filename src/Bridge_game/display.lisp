;;;; display.lisp — Text-Based Display for Bridge Game
;;;;
;;;; Renders the bridge table, hands, bidding history, and trick play
;;;; in a clear text-based format suitable for terminal play.

(in-package #:bridge)

;;; ============================================================
;;; CARD AND HAND DISPLAY
;;; ============================================================

(defun display-card (card &optional (stream t))
  "Display a single card."
  (format stream "~A" card))

(defun display-hand-inline (hand &optional (stream t))
  "Display a hand as a single line of cards."
  (format stream "~{~A~^ ~}" hand))

(defun display-hand-by-suit (hand &optional (stream t))
  "Display a hand organized by suit, one suit per line."
  (loop for suit from +spades+ downto +clubs+
        for cards = (hand-suit-cards hand suit)
        do (format stream "  ~A: ~{~A~^ ~}~%"
                   (aref +suit-symbols+ suit)
                   (mapcar (lambda (c)
                             (gethash (card-rank c) +rank-names+))
                           (sort (copy-list cards)
                                 (lambda (a b) (> (card-rank a) (card-rank b))))))))

(defun hand-summary (hand &optional (stream t))
  "Display hand with HCP and shape analysis."
  (display-hand-by-suit hand stream)
  (format stream "  HCP: ~D  Shape: ~{~D-~}  Total: ~D~%"
          (hand-hcp hand)
          (hand-shape hand)
          (hand-total-points hand)))

;;; ============================================================
;;; BOARD / TABLE DISPLAY
;;; ============================================================

(defun pad-string (str width &optional (align :left))
  "Pad a string to a given width."
  (let ((len (length str)))
    (if (>= len width)
        (subseq str 0 width)
        (case align
          (:left  (concatenate 'string str (make-string (- width len) :initial-element #\Space)))
          (:right (concatenate 'string (make-string (- width len) :initial-element #\Space) str))
          (:center
           (let* ((left-pad (floor (- width len) 2))
                  (right-pad (- width len left-pad)))
             (concatenate 'string
                          (make-string left-pad :initial-element #\Space)
                          str
                          (make-string right-pad :initial-element #\Space))))))))

(defun suit-line-string (hand suit)
  "Get a string representation of a suit in a hand."
  (let ((cards (hand-suit-cards hand suit)))
    (if cards
        (format nil "~A ~{~A~^ ~}"
                (aref +suit-symbols+ suit)
                (mapcar (lambda (c) (gethash (card-rank c) +rank-names+))
                        (sort (copy-list cards)
                              (lambda (a b) (> (card-rank a) (card-rank b))))))
        (format nil "~A ---" (aref +suit-symbols+ suit)))))

(defun display-board (hands &key (human-player +south+)
                                  (dummy nil)
                                  (current-trick nil)
                                  (declarer nil)
                                  (vulnerability nil)
                                  (stream t))
  "Display the bridge table with all four hands.
   Shows human player's hand fully; other hands are hidden
   unless they are the dummy."
  (declare (ignore declarer))
  (let* ((n-hand (aref hands +north+))
         (e-hand (aref hands +east+))
         (s-hand (aref hands +south+))
         (w-hand (aref hands +west+))
         (show-north (or (= human-player +north+) (eql dummy +north+)))
         (show-east  (or (= human-player +east+)  (eql dummy +east+)))
         (show-south (or (= human-player +south+) (eql dummy +south+)))
         (show-west  (or (= human-player +west+)  (eql dummy +west+)))
         (width 60))

    ;; Header
    (format stream "~%~A~%" (make-string width :initial-element #\═))
    (format stream "~A~%"
            (pad-string "♠ ♥ ♦ ♣  BRIDGE TABLE  ♣ ♦ ♥ ♠" width :center))
    (when vulnerability
      (format stream "~A~%"
              (pad-string (format nil "Vul: ~A" vulnerability) width :center)))
    (format stream "~A~%" (make-string width :initial-element #\─))

    ;; North (top)
    (format stream "~A~%"
            (pad-string (format nil "NORTH~A"
                                (if (= human-player +north+) " (You)" ""))
                        width :center))
    (if show-north
        (loop for suit from +spades+ downto +clubs+
              do (format stream "~A~%"
                         (pad-string (suit-line-string n-hand suit) width :center)))
        (format stream "~A~%"
                (pad-string (format nil "[~D cards]" (length n-hand)) width :center)))

    (format stream "~%")

    ;; West and East (middle row)
    (let ((west-lines (if show-west
                         (loop for suit from +spades+ downto +clubs+
                               collect (suit-line-string w-hand suit))
                         (list (format nil "[~D cards]" (length w-hand)))))
          (east-lines (if show-east
                         (loop for suit from +spades+ downto +clubs+
                               collect (suit-line-string e-hand suit))
                         (list (format nil "[~D cards]" (length e-hand)))))
          (trick-lines (if current-trick
                           (list (format nil "Trick: ~{~A~^ ~}" current-trick))
                           nil)))

      ;; West label
      (format stream "~A~A~A~%"
              (pad-string (format nil "WEST~A"
                                  (if (= human-player +west+) " (You)" ""))
                          20)
              (pad-string "" 20 :center)
              (pad-string (format nil "EAST~A"
                                  (if (= human-player +east+) " (You)" ""))
                          20))

      ;; Side-by-side display
      (let ((max-lines (max (length west-lines) (length east-lines) 1)))
        (loop for i from 0 below max-lines
              for wl = (if (< i (length west-lines)) (nth i west-lines) "")
              for el = (if (< i (length east-lines)) (nth i east-lines) "")
              for tl = (if (and trick-lines (< i (length trick-lines)))
                           (nth i trick-lines) "")
              do (format stream "~A~A~A~%"
                         (pad-string wl 20)
                         (pad-string tl 20 :center)
                         (pad-string el 20)))))

    (format stream "~%")

    ;; South (bottom)
    (format stream "~A~%"
            (pad-string (format nil "SOUTH~A"
                                (if (= human-player +south+) " (You)" ""))
                        width :center))
    (if show-south
        (loop for suit from +spades+ downto +clubs+
              do (format stream "~A~%"
                         (pad-string (suit-line-string s-hand suit) width :center)))
        (format stream "~A~%"
                (pad-string (format nil "[~D cards]" (length s-hand)) width :center)))

    (format stream "~A~%" (make-string width :initial-element #\═))))

;;; ============================================================
;;; BIDDING DISPLAY
;;; ============================================================

(defun display-bidding-history (bid-history dealer &optional (stream t))
  "Display the bidding in a table format."
  (let* ((players (vector "North" "East" "South" "West"))
         (col-width 10))
    (format stream "~%~A~%" (make-string 44 :initial-element #\─))
    (format stream " BIDDING AUCTION~%")
    (format stream "~A~%" (make-string 44 :initial-element #\─))

    ;; Header row
    (loop for i from 0 below 4
          do (format stream "~A" (pad-string (aref players i) col-width)))
    (format stream "~%")
    (format stream "~A~%" (make-string 40 :initial-element #\─))

    ;; Skip columns before dealer
    (let ((col dealer)
          (bids (reverse bid-history)))  ; oldest first
      ;; Initial padding for dealer position
      (loop for i from 0 below dealer
            do (format stream "~A" (pad-string "" col-width)))
      (setf col dealer)

      ;; Print bids
      (dolist (bid bids)
        (format stream "~A" (pad-string (format nil "~A" bid) col-width))
        (incf col)
        (when (= (mod col 4) 0)
          (format stream "~%")))
      (when (/= (mod col 4) 0)
        (format stream "~%")))

    (format stream "~A~%" (make-string 44 :initial-element #\─))))

;;; ============================================================
;;; TRICK DISPLAY
;;; ============================================================

(defun display-current-trick (trick lead-player &optional (stream t))
  "Display the current trick being played."
  (format stream "~%  Current trick:~%")
  (loop for i from 0 below (length trick)
        for player = (mod (+ lead-player i) 4)
        for card = (nth i trick)
        do (format stream "    ~A: ~A~%"
                   (aref +player-names+ player)
                   card)))

(defun display-trick-count (ns-tricks ew-tricks &optional (stream t))
  "Display running trick count."
  (format stream "  N-S tricks: ~D    E-W tricks: ~D~%" ns-tricks ew-tricks))

;;; ============================================================
;;; GAME STATUS DISPLAY
;;; ============================================================

(defun display-contract (contract-bid declarer doubled &optional (stream t))
  "Display the final contract."
  (format stream "~%  Contract: ~A by ~A~A~%"
          (bid-name contract-bid)
          (aref +player-names+ declarer)
          (case doubled
            (1 " (Doubled)")
            (2 " (Redoubled)")
            (t ""))))



(defun display-welcome (&optional (stream t))
  "Display welcome message."
  (format stream "~%")
  (format stream "╔══════════════════════════════════════════════╗~%")
  (format stream "║          RUBBER BRIDGE — AI Edition          ║~%")
  (format stream "║                                              ║~%")
  (format stream "║  You play as South.                          ║~%")
  (format stream "║  North is your AI partner.                   ║~%")
  (format stream "║  East-West are AI opponents.                 ║~%")
  (format stream "║                                              ║~%")
  (format stream "║  Scoring: Rubber bridge (first to 2 games)   ║~%")
  (format stream "║  Bidding: Standard American (simplified)     ║~%")
  (format stream "║  Play:   Heuristic AI                        ║~%")
  (format stream "╚══════════════════════════════════════════════╝~%"))
