#lang racket

(require "boarddata.rkt")
(require "shuffle.rkt")
(require "sound.rkt")
(provide (all-defined-out))

;;Matching Procedure
(define c1 #f)
(define (m c)
  (cond ((not c1)
         ;;This is the first card flipped
         (begin(set! c1 c)
               (send board flip-card c)
               (play-flip-sound c)))
        (c1
         ;;This is when the second card is flipped
         (send board flip-card c)
         (if (and (equal? (send c1 get-suit) (send c get-suit))
                  (equal? (send c1 get-value) (send c get-value)))
             (send board remove-cards (list c1 c))
             (begin (send board flip-cards (list c1 c))
                    (set! c1 #f) (shuffle-counter))))
        (else (displayln "Error"))))
