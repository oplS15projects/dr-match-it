#lang racket
;******************************************************************************
; Dr. Match-It -- OPL Final Project Spring 2015
; Team: Pebble BoyZ
; File: main.rkt
;
; Main game initialization file
; Will integrate game, sound, GUI components and handle game initialization
;******************************************************************************

(require "boarddata.rkt")
(require "matching.rkt")

(require games/cards)
(require racket/gui)

(send board show #t)
(send board set-single-click-action m)
