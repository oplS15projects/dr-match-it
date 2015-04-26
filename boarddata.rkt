;******************************************************************************
; Dr. Match-It -- OPL Final Project Spring 2015
; Team: Pebble BoyZ
; File: cardgame.rkt
;
; Implementation of cardgame rules & functionality
; See "main.rkt" for initialization
; For testing, make another file with (include "cardgame.rkt")
; in the working directory and call into this one
;******************************************************************************

#lang racket

(provide (all-defined-out))
(require games/cards)
(require racket/gui)
;;Mismatches before shuffle
(define SHUFFLE-COUNT 5)
;;#Rows and Columns
(define WIDTH 4)
(define HEIGHT 3)
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