#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                      -+- CONFIG -+-                      *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(provide
 venster-hoogte
 venster-breedte
 titel
 bullet-refresh-rate
 power-up-refresh-rate
 fleet-refresh-rate
 horizontal-speed-monster
 vertical-speed-monster
 correction
 bullet-speed
 rocket-speed)

(define titel "SPACE INVADERS v3")

; De waarden van deze variabelen zijn in absolute pixels.
; Sprites zijn gemaakt voor een spelvenster van minimum 700 pixels in breedte.

(define venster-hoogte 600)
(define venster-breedte 700)

; Deze formule zal gebruikt worden om de sprites op hun correcte plaats te zetten.

(define (correction tile-dimension screen-dimension)
  (/ tile-dimension screen-dimension))

; Er wordt een getal bepaald om de snelheid waarmee de bullet-loop, power-up-loop
; en de fleet-loop binnen het spel-adt wordt aangeroepen.

(define bullet-refresh-rate 10)
(define fleet-refresh-rate 500)
(define power-up-refresh-rate 1)

; Deze waarden geven aan welke afstand per keer de fleet zich mag voortbewegen.

(define horizontal-speed-monster 0.02)
(define vertical-speed-monster 0.03)

; Deze waarde geeft weer hoe ver de bullet zich per keer verplaatst.

(define bullet-speed 0.03)

; Deze waarde geeft weer hoe ver de rocket zich per keer verplaatst.

(define rocket-speed 0.02)
