#lang racket
;*----------------------------------------------------------*
;*                 //Space Invaders launch//                *
;*                      Senne Deproost                      *
;*                           V 3                            *
;*----------------------------------------------------------*

(require "spel-adt.rkt")

(define spel (maak-spel-adt))

(spel 'start-menu)
