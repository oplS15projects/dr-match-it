;******************************************************************************
; Dr. Match-It -- OPL Final Project Spring 2015
; Team: Pebble BoyZ
; File: sound.rkt
;
; Implementation of sound engine 
; See "main.rkt" for initialization
; For testing, make another file with (include "sound.rkt")
; in the working directory and call into this one
;******************************************************************************
#lang racket

(provide (all-defined-out))
(require rsound)


;; Utilities
(define (symrand) (if (< 1 (random 2)) (random) (- (random))))

(define 60th 735)

;----GENERATORS----------------------------------------------------------------
; a simple falling tone - falls one octave
; to be chained together later to make a "you lose" sort of sound?
(define (make-drop-tone pitch frames volume)
  (build-sound frames (lambda (f)
                        (* volume
                           (sin (* 2 pi
                                   (- pitch
                                      (* (/ f (- frames 1)) 
                                         (* 0.50 pitch))) ; lerps from start down one octave
                                   (/ f FRAME-RATE)))))))

(define (make-pulse pitch frames volume duty)
  (signal->rsound frames (network ()
                                  [p <= pulse-wave duty pitch]
                                  [a <= lpf/dynamic 0.6 p]
                                  [out = (* volume a)])))

(define (make-pluck pitch attack sustain volume)
  (rs-append (make-pulse pitch attack volume 0.5)
             (make-pulse pitch sustain (/ volume 2) 0.25)))

;----FILTERS-------------------------------------------------------------------

(define (add-noise sound max-offset)
  (rs-filter sound
             (network (f)
                      [out = (+ f (* max-offset (symrand)))])))
(define (noisify sound)
  (rs-filter sound
             (network (f)
                      [out = (* f (symrand))])))

;----INITIALIZATION------------------------------------------------------------
(define flip-tritone  (rs-append* (list (make-pluck 659.25 60th 7350 0.05)
                                        (silence 1323)
                                        (make-pluck 830.61 60th 7350 0.05)
                                        (silence 1323)
                                        (make-pluck 987.77 60th 7350 0.05))))

;----INTEGRATION PLAYERS-------------------------------------------------------

(define (play-flip-sound card)
  (play flip-tritone))