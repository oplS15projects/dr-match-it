#lang racket

(provide (all-defined-out))

(require games/cards)
(require racket/gui)
(require "sound.rkt")
(define Multiplier 1)
;;#Rows and Columns
(define WIDTH 4)
(define HEIGHT 3)
(define MAX (/ (* WIDTH HEIGHT) 2))
;;Creating Board with # of rows and Columns in mind
(define board (make-table "Dr.Match It" (+ 3 WIDTH) (+ 1 HEIGHT)))
(send board show #t)
(send board set-double-click-action #f)
;;Geting true board width and height
(define w (send board table-width))
(define h (send board table-height))
;;Background
(define bg (make-background-region 0 0 w h (lambda (dc x y w h)
                                             (send dc set-brush "CadetBlue" 'solid)
                                             (send dc draw-rectangle 0 0 800 800))))
(send board add-region bg)
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
                    (set! MAX (- MAX 1))
                    (set! c1 #f)
                    (if (= MAX 0)
                        (update-final-score (* 10 Multiplier))
                        (update-score (* 10 Multiplier)))
                    (set! Multiplier (+ Multiplier .5)))
             ;;This is when there is an incorrect flip
             (begin (send board pause .5) (send board flip-cards (list c1 c))
                    (set! c1 #f)
                    (set! Multiplier 1))))
        (else (displayln "Error"))))
 
;;Create Scoring area
(define current-score 0)
(define (make-score-area score)
  (make-background-region (- score-x 70) score-y (+ 100 cw) 70 (lambda (dc x y w h)
                                                               (send dc set-font (make-font #:size 13 #:family 'swiss 
                                                                                            #:style 'slant #:weight 'bold))
                                                               (send dc set-text-foreground "DarkSlateGray")
                                                               (send dc draw-text (format
                                                                                   "Score: ~a"
                                                                                   (number->string score))
                                                                     (- score-x 50) (+ score-y 20)))))
(define score-area (make-score-area 0))
;;Final Score area
(define (make-final-area score)
  (make-background-region 100 100 500 100
               (lambda (dc x y w h)
                                                               (send dc set-font (make-font #:size 20 #:family 'swiss 
                                                                                            #:style 'slant #:weight 'bold))
                                                               (send dc set-text-foreground "DarkSlateGray")
                                                               (send dc draw-text (format
                                                                                   "Final Score: ~a"
                                                                                   (number->string score))
                                                                     150 100))))
(define (update-final-score score)
  (send board begin-card-sequence)
  (send board remove-region score-area)
  (set! score-area (make-final-area (+ score current-score)))
  (send board add-region score-area)
  (send board end-card-sequence))
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