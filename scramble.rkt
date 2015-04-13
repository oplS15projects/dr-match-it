#lang racket
(require games/cards)

(define (x-inc x) (+ x 72))
(define (y-inc y) (+ y 98))

;; offset to place cards in a row

(define (offset i) 
  (cond((equal? i 0) (values 0 0))
       (else (values (* 72 (modulo i 13)) (* 98 (quotient i 13))))))

(define deck (make-deck))
(define table (make-table "Table" 14 5))
(send table add-cards deck 0 0 offset) ;; add cards to table


;; Below is the whole shuffle routine with it's sub functions
;; pretty-shuffle is where it all really happens

;; Send everyone home

(define (move-home card)
 (send table move-card card 0 0)
)

;; This part "shuffles" the cards on home - really just 
;; moves the cards over and back on top

(define count1 0)
 
(define (shuff deck) 
  (displayln "Shuffling!") 
  (set! count1 (+ count1 1)) ;; inc how many times we "shuffle"
  (send table move-card (car deck) 72 0) ;; move over
  (send table move-card (car deck) 0 0) ;; put back
  (check)) ;; check if we did this 5 times

(define (check)
  (cond ((< count1 5) (shuff deck)) ;; check if it's 5 or "shuffle" again
        (else (set! count1 (- count1 count1))))) ;; reset for next time
 
(define (shuffle-animation deck) ;; call the stuff above
  (shuff deck))

;; End of shuffle at home

;; Begin all shuffle functions - This is where it all happens

(define (pretty-shuffle vars)
  (send table cards-face-down deck) ;; flip cards that are face up
  (displayln "Moving cards back home!")
  (map move-home (cdr deck)) ;; move all cards to home
  (shuffle-list deck 7) ;; actually shuffle
  (shuffle-animation deck) ;; make it look like it's shuffling
  (displayln "Setting up cards again!")
  (send table move-cards deck 0 0 offset)) ;; fix cards, now shuffles.

;; Done with shuffle

;; Just to make sure it's flipping cards back over

(send table card-face-up (car deck))
(send table card-face-up (car (cdr deck)))
(send table card-face-up (car (cdr (cdr deck))))

;; double click to shuffle for now

(send table set-double-click-action pretty-shuffle)

(send table show #t) 
