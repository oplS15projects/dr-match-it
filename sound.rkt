;******************************************************************************
; Dr. Match-It -- OPL Final Project Spring 2015
; Team: Pebble BoyZ
; File: sound.rkt
;
; Implementation of sound engine 
; See "main.rkt" for initialization
;
;******************************************************************************
#lang racket

(provide (all-defined-out))
(require rsound)


;; Utilities
(define MASTER-VOL 0.25)
(define (symrand) (if (< 1 (random 2)) (random) (- (random))))

;----GENERATORS----------------------------------------------------------------
;; a simple falling tone - falls one octave
;; to be chained together later to make a "you lose" sort of sound?
(define (make-drop-tone pitch frames volume)
  (build-sound frames (lambda (f)
                        (* volume
                           (sin (* 2 pi
                                   (- pitch
                                      (* (/ f (- frames 1)) 
                                         (* 0.50 pitch))) ; lerps from start down one octave
                                   (/ f FRAME-RATE)))))))


;; here we have many generators for pulse waves to take advantage of
;; duty cycle modulation for a NES-ish retro sound
(define (make-pulse pitch frames volume duty)
  (signal->rsound frames (network ()
                                  [p <= pulse-wave duty pitch]
                                  [a <= lpf/dynamic 0.6 p] ; lpf to remove aliasing
                                  [out = (* volume a)])))

(define (make-mod-pulse pitch frames volume interval)
  (define (maybe-change current loop)
    (if (= loop 0) (cond ((= current .125) .25)
                         ((= current .25) .5)
                         ((= current .5) .125)
                         (else .125))
        current))
  (signal->rsound frames (network ()
                                  [f <= (loop-ctr interval 1)]
                                  [duty = (maybe-change (prev duty 0.125) f)]
                                  [p <= pulse-wave duty pitch]
                                  [a <= lpf/dynamic 0.6 p]
                                  [out = (* volume a)])))

(define (make-mod-sweep start-pitch end-pitch frames volume interval)
  (define (maybe-change current loop)
    (if (= loop 0) (cond ((= current .125) .25)
                         ((= current .25) .5)
                         ((= current .5) .125)
                         (else .125))
        current))
  (signal->rsound frames (network ()
                                  [f <= (loop-ctr interval 1)]
                                  [g <= (simple-ctr 0 1)]
                                  [duty = (maybe-change (prev duty 0.125) f)]
                                  [p <= pulse-wave duty (+ start-pitch (* (/ g frames) (- end-pitch start-pitch)))]
                                  [a <= lpf/dynamic 0.6 p]
                                  [out = (* volume a)])))

(define (make-pluck pitch attack sustain volume)
  (rs-append (make-pulse pitch attack volume 0.5)
             (make-pulse pitch sustain (/ volume 2) 0.25)))


;; given a short duration, this gives a semi-percussive bell sound
(define (make-click pitch frames volume)
  (signal->rsound frames (network ()
                                  [s1 <= sine-wave pitch]
                                  [s2 <= sine-wave (* 0.83 pitch)]
                                  [s3 <= sine-wave (* 2.22 pitch)]
                                  [prod = (+ (* 0.33 s1) (* 0.33 s2) (* 0.33 s3))]
                                  [th <= thresh 1.0 prod]
                                  [out = (* volume th)])))

;; use the symmetric -1 to 1 random utility above to generate noise
(define (make-noise frames volume)
  (build-sound frames (lambda (f)
                        (* volume (symrand)))))

;----FILTERS/WINDOWS-----------------------------------------------------------

;; add noise via multiplication so we stick to volume constraints
(define (noisify sound)
  (rs-filter sound
             (network (f)
                      [out = (* f (symrand))])))


;; so we can apply a lowpass filter directly to an rsound
(define (lowpass cutoff sound)
  (rs-filter sound
             (network (f)
                      [out <= lpf/dynamic cutoff f])))
;----INITIALIZATION------------------------------------------------------------

;; here we combine all the stuff above to make the ingame sounds

(define you-lose (rs-append* (list (make-drop-tone 880 14700 MASTER-VOL)
                                   (make-drop-tone 660 14700 MASTER-VOL)
                                   (make-drop-tone 440 14700 MASTER-VOL))))

(define you-win (rs-scale (* 3 MASTER-VOL) (rs-append* (list (rs-overlay* (list (rs-scale 0.33
                                                                                    (synth-note "vgame" 49 64 4410))
                                                                          (rs-scale 0.33
                                                                                    (synth-note "vgame" 49 68 4410))
                                                                          (rs-scale 0.33
                                                                                    (synth-note "vgame" 49 71 4410))))
                                                       (silence 2205)
                                                       (rs-overlay* (list (rs-scale 0.33
                                                                                    (synth-note "vgame" 49 64 44100))
                                                                          (rs-scale 0.33
                                                                                    (synth-note "vgame" 49 68 44100))
                                                                          (rs-scale 0.33
                                                                                    (synth-note "vgame" 49 71 44100))))))))

(define shuff-sound (rs-append* (list (lowpass 0.6 (make-noise 2000 (/ MASTER-VOL 2)))
                                      (make-tone (midi-note-num->pitch 92) MASTER-VOL 2800))))

(define wrong-match (make-mod-sweep 110 55 11025 MASTER-VOL (/ 44100 50))) 

(define correct-match (rs-scale MASTER-VOL (rs-append* (list (rs-overlay* (list (make-mod-pulse 660 6300 MASTER-VOL (/ 44100 60))
                                                                                (make-mod-pulse 880 6300 MASTER-VOL (/ 44100 60))
                                                                                (make-mod-pulse 1100 6300 MASTER-VOL (/ 44100 60))
                                                                                (make-mod-pulse 1320 6300 MASTER-VOL (/ 44100 60))))
                                                             (silence 735)
                                                             (rs-overlay* (list (make-mod-pulse 660 6300 (/ MASTER-VOL 2) (/ 44100 60))
                                                                                (make-mod-pulse 880 6300 (/ MASTER-VOL 2) (/ 44100 60))
                                                                                (make-mod-pulse 1100 6300 (/ MASTER-VOL 2) (/ 44100 60))
                                                                                (make-mod-pulse 1320 6300 (/ MASTER-VOL 2) (/ 44100 60))))
                                                             (silence 735)
                                                             (rs-overlay* (list (make-mod-pulse 660 6300 (/ MASTER-VOL 3) (/ 44100 60))
                                                                                (make-mod-pulse 880 6300 (/ MASTER-VOL 3) (/ 44100 60))
                                                                                (make-mod-pulse 1100 6300 (/ MASTER-VOL 3) (/ 44100 60))
                                                                                (make-mod-pulse 1320 6300 (/ MASTER-VOL 3) (/ 44100 60))))
                                                             (silence 735)
                                                             (rs-overlay* (list (make-mod-pulse 660 6300 (/ MASTER-VOL 4) (/ 44100 60))
                                                                                (make-mod-pulse 880 6300 (/ MASTER-VOL 4) (/ 44100 60))
                                                                                (make-mod-pulse 1100 6300 (/ MASTER-VOL 4) (/ 44100 60))
                                                                                (make-mod-pulse 1320 6300 (/ MASTER-VOL 4) (/ 44100 60))))))))

(define flip1 (rs-append* (list (make-pluck 659.25 735 7350 MASTER-VOL)
                                (silence 1323)
                                (make-pluck 830.61 735 7350 MASTER-VOL)
                                (silence 1323)
                                (make-pluck 987.77 735 7350 MASTER-VOL))))

(define flip2 (rs-append* (list (make-mod-pulse (midi-note-num->pitch 84) (/ 44100 6) MASTER-VOL (/ 44100 50))
                                (silence 1323)
                                (make-mod-pulse (midi-note-num->pitch 88) (/ 44100 6) MASTER-VOL (/ 44100 50))
                                (silence 1323)
                                (make-mod-pulse (midi-note-num->pitch 91) (/ 44100 6) MASTER-VOL (/ 44100 50))
                                (silence 735))))

(define flip3 (rs-scale MASTER-VOL (rs-overlay* (list (make-mod-pulse 440 11025 MASTER-VOL (/ 44100 50))
                                                      (make-mod-pulse 1320 11025 MASTER-VOL (/ 44100 50))
                                                      (make-mod-pulse 1760 11025 MASTER-VOL (/ 44100 50))))))

(define flip4 (make-mod-sweep 880 1760 14700 MASTER-VOL (/ 44100 50)))

(define flip5 (make-drop-tone 220 11025 MASTER-VOL))

(define flip6 (rs-append* (list (make-mod-sweep 220 440 5525 MASTER-VOL (/ 44100 75))
                                (silence 735)
                                (make-mod-sweep 330 660 5525 MASTER-VOL (/ 44100 75)))))
;----INTEGRATION-PLAYERS-------------------------------------------------------

;; find the right sound to play given a flipped card
(define (play-flip-sound card)
  (cond ((equal? (send card get-value) 1) (begin (play flip1) (sleep 0.2)))
        ((equal? (send card get-value) 2) (begin (play flip2) (sleep 0.2)))
        ((equal? (send card get-value) 3) (begin (play flip3) (sleep 0.2)))
        ((equal? (send card get-value) 4) (begin (play flip4) (sleep 0.2)))
        ((equal? (send card get-value) 5) (begin (play flip5) (sleep 0.2)))
        ((equal? (send card get-value) 6) (begin (play flip6) (sleep 0.2)))
        (else (error "play-flip-sound: invalid card value"))))

;; play event-based sounds -- wrong matches, winning the game, etc.
(define (play-event-sound event)
  (cond ((eq? 'shuffle event) (begin (play shuff-sound) (sleep 0.2)))
        ((eq? 'lose event) (begin (play you-lose) (sleep 0.2)))
        ((eq? 'wrong event) (begin (play wrong-match) (sleep 0.2)))
        ((eq? 'match event) (begin (play correct-match) (sleep 0.2)))
        ((eq? 'win event) (begin (play you-win) (sleep 0.2)))
        (else (error "play-event-sound: invalid event"))))

