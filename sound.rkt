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
(require rsound)

(define (symrand) (if (< 1 (random 2)) (random) (- (random))))

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


;----FILTERS-------------------------------------------------------------------

(define (add-noise sound max-offset)
  (rs-filter sound
             (network (f)
                      [out = (+ f (* max-offset (symrand)))])))
(define (noisify sound)
  (rs-filter sound
             (network (f)
                      [out = (* f (symrand))])))