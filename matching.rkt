#lang racket

(provide (all-defined-out))

(require games/cards)
(require racket/gui)
(require "sound.rkt")

;;#Rows and Columns
(define WIDTH 4)
(define HEIGHT 4)
;;Creating Board with # of rows and Columns in mind
(define board (make-table "Dr.Match It" (+ 3 WIDTH) (+ 1 HEIGHT)))
(send board show #t)
(send board set-double-click-action #f)
;;Geting true board width and height
(define w (send board table-width))
(define h (send board table-height))
;;Building deck with custom cards
(define deck
  (let ([cards (map (lambda (name value)
                      (let ([bm (make-object
                                 bitmap%
                                 (build-path
                                  (current-directory) "img"
                                  (format "~a.png" name)))]
                            [back (make-object
                                      bitmap%
                                    (build-path
                                     (current-directory) "img"
                                     "cardback.png"))])
                        (make-card bm back 0 value)))
                    (list "cardfront_1" "cardfront_2" "cardfront_3"
                          "cardfront_4" "cardfront_5" "cardfront_6")
                    (list 1 2 3 4 5 6))])
    (append cards (map (lambda (c) (send c copy)) cards))))
;;Get Card width and height
(define cw (send (car deck) card-width))
(define ch (send (car deck) card-height))
 
(define dx (/ cw (+ 2 WIDTH)))
(define dy (/ ch (+ 1 HEIGHT)))
 
(define match-x (- w cw dx))
(define match-y dy)
;;Score area
(define score-x (- w cw dx))
(define score-y dy)
;;Restrict user from moving cards, only letting them flip
(for-each (lambda (card)
            (send card user-can-move #f)
            (send card user-can-flip #t))
          deck)
;;Adding Deck to board
(send board add-cards deck match-x match-y)
(set! deck (shuffle-list deck 7))
  (send board stack-cards deck)
  (send board move-cards deck 0 0
        (lambda (pos)
          (let ([i (modulo pos WIDTH)]
                [j (quotient pos WIDTH)])
            (values (+ dx (* i (+ cw dx)))
                    (+ dy (* j (+ ch dy)))))))
;;Matching Procedure
(define c1 #f)
(define (m c)
  (cond ((not c1)
         ;;This is the first card flipped
         (begin(set! c1 c)
               (send board flip-card c)
               (play-flip-sound c)))
        ;;This is preventing players from double clicking already face up cards
        ((not (send c face-down?))
         (set! c1 c))
        (c1
         ;;This is when the second card is flipped
         (send board flip-card c)
         (if (and (equal? (send c1 get-suit) (send c get-suit))
                  (equal? (send c1 get-value) (send c get-value)))
             ;;This is when theres a match
             (begin (play-flip-sound c)
                    (send board pause .5) 
                    (send board remove-cards (list c1 c)) 
                    (set! c1 #f)
                    (update-score 10))
             ;;This is when there is an incorrect flip
             (begin (send board pause .5) (send board flip-cards (list c1 c))
                    (set! c1 #f))))
        (else (displayln "Error"))))
 
;;Create Scoring area
(define current-score 0)
(define (make-score-area score)
  (make-region score-x score-y (+ 3 cw) 22
               (format
                "Score: ~a"
                (number->string score))
               #f))
(define score-area (make-score-area 0))
;;Update Score Procedure
(define (update-score score)
  (set! current-score (+ current-score score))
  (send board begin-card-sequence)
  (send board remove-region score-area)
  (set! score-area (make-score-area current-score))
  (send board add-region score-area)
  (send board end-card-sequence))
(send board add-region score-area)
 
(send board set-single-click-action m)