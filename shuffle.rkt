#lang racket

(require games/cards)
(require racket/gui)
(require "boarddata.rkt")
(provide (all-defined-out))

;;Utility functions

;; Offsets for where to throw the cards when we place them back on the table

(define (offset i) 
  (cond((equal? i 0) (values 0 0))
       (else (values (* 85 (modulo i 4)) (* 115 (quotient i 4))))))

;; Check if I should scramble or not

(define scount 0)
(define (shuffle-counter)
    (cond((equal? scount SHUFFLE-COUNT)
         (begin (pretty-shuffle deck) (set! scount (- scount scount)) (displayln "reset scount")))
         (else (begin (set! scount (+ scount 1)) (displayln "add 1 to scount")))))
         

;; Below is the whole shuffle routine with it's sub functions
;; pretty-shuffle is where it all really happens

;; Send everyone home

(define (move-home card)
 (send board move-card card 12 24)
)

;; This part "shuffles" the cards on home - really just 
;; moves the cards over and back on top

(define count1 0)
 
(define (shuff deck) 
  (set! count1 (+ count1 1)) ;; inc how many times we "shuffle"
  (send board move-card (cadr (send board all-cards))  72 24) ;; move over
  (send board move-card (cadr (send board all-cards)) 12 24) ;; put back
  (check)) ;; check if we did this 5 times

(define (check)
  (cond ((< count1 5) (shuff deck)) ;; check if it's 5 or "shuffle" again
        (else (set! count1 (- count1 count1))))) ;; reset for next time
 
(define (shuffle-animation deck) ;; call the stuff above
  (shuff deck))

;; End of shuffle at home

;; Begin all shuffle functions - This is where it all happens

(define (pretty-shuffle vars)
  (send board cards-face-down deck) ;; flip cards that are face up
  (map move-home (cdr deck)) ;; move all cards to home
  (shuffle-animation deck) ;; make it look like it's shuffling
  (send board move-cards (shuffle-list deck 10) 12 24 offset)) ;; Place shuffled deck

;; Done with shuffle
