#lang racket

(provide (all-defined-out))

(require games/cards)
(require racket/gui)
(require "sound.rkt")
(require "boarddata.rkt")
(require "shuffle.rkt")
(define Multiplier 1)
(define MAX (/ (* WIDTH HEIGHT) 2))

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
                    (play-event-sound 'match)
                    (send board remove-cards (list c1 c))
                    (set! MAX (- MAX 1))
                    (set! c1 #f)
                    (if (= MAX 0)
                        (update-final-score (* 10 Multiplier))
                        (update-score (* 10 Multiplier)))
                    (set! Multiplier (+ Multiplier .5)))
             ;;This is when there is an incorrect flip
             (begin (play-flip-sound c)
                    (send board pause .5)
                    (play-event-sound 'wrong)
                    (send board flip-cards (list c1 c))
                    (set! c1 #f)
                    (set! Multiplier 1)
                    (shuffle-counter))))
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
  (send board end-card-sequence)
  (play-event-sound 'win))
;;Update Score Procedure
(define (update-score score)
  (set! current-score (+ current-score score))
  (send board begin-card-sequence)
  (send board remove-region score-area)
  (set! score-area (make-score-area current-score))
  (send board add-region score-area)
  (send board end-card-sequence))
(send board add-region score-area)
