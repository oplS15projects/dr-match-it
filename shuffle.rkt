#lang racket

(require games/cards)
(require racket/gui)
(require "boarddata.rkt")
(provide (all-defined-out))

;;Utility functions

(define (offset i) 
  (cond((equal? i 0) (values 0 0))
       (else (values (* 85 (modulo i 4)) (* 115 (quotient i 4))))))

(define scount 0)
(define (shuffle-counter)
    (cond((equal? scount 5)
         (begin (pretty-shuffle deck) (set! scount (- scount scount))))
         (else (set! scount (+ scount 1)))))
         

;; Below is the whole shuffle routine with it's sub functions
;; pretty-shuffle is where it all really happens

;; Send everyone home

(define (move-home card)
 (send board move-card card 0 0)
)

;; This part "shuffles" the cards on home - really just 
;; moves the cards over and back on top

(define count1 0)
 
(define (shuff deck) 
  (displayln "Shuffling!") 
  (set! count1 (+ count1 1)) ;; inc how many times we "shuffle"
  (send board move-card (caddr deck) 72 0) ;; move over
  (send board move-card (caddr deck) 0 0) ;; put back
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
  (displayln "Moving cards back home!")
  (map move-home (cdr deck)) ;; move all cards to home
  (shuffle-list deck 10) ;; actually shuffle
  (shuffle-animation deck) ;; make it look like it's shuffling
  (displayln "Setting up cards again!")
  (send board move-cards deck 0 0 offset)) ;; fix cards, now shuffles.

;; Done with shuffle
