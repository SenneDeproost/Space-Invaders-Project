#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                      -+- MENU'S -+-                      *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(require "menu-part-adt.rkt")
(require "config.rkt")

(provide start-menu-list gameover-menu-list levelcomplete-menu-list pauze-menu-list highscore-insert-menu-list highscores-menu-list)

; Variabele die de correcte x-waarde berekent in het abstract vlak van het venster.
; Deze werkt enkel bij een spelvenster van minstens 700 pixels breed en menu-parts van 700 pixels breed.

(define x-correctie (/ (/ (- venster-breedte 700) 2) venster-breedte))

;Fleet wordt voorgesteld als een lijst van monster-ADT's

; Start menu

 (define start-menu-list
   (list
    (maak-menu-part-adt "sprites/title.jpg" x-correctie  0)
    (maak-menu-part-adt "sprites/startinstruction.jpg" x-correctie 0.5)
    (maak-menu-part-adt "sprites/showhighscoresinstruction.jpg" x-correctie 0.6)
    (maak-menu-part-adt "sprites/name.jpg" x-correctie 0.9)))

; Pauze menu

(define pauze-menu-list
  (list
   (maak-menu-part-adt "sprites/paused.jpg" x-correctie 0.3)
   (maak-menu-part-adt "sprites/pauzecontinueinstruction.jpg" x-correctie 0.5)))

; Game over menu

(define gameover-menu-list
  (list
   (maak-menu-part-adt "sprites/gameover.jpg" x-correctie 0.3)
   (maak-menu-part-adt "sprites/restartinstruction.jpg" x-correctie 0.5)))

; Insert highscore menu

(define highscore-insert-menu-list
  (list
   (maak-menu-part-adt "sprites/insertname.jpg" x-correctie 0.3)
   (maak-menu-part-adt "sprites/nameline.jpg" x-correctie 0.5)))
   
; Level complete menu

(define levelcomplete-menu-list
  (list
   (maak-menu-part-adt "sprites/levelcomplete.jpg" x-correctie 0.3)
   (maak-menu-part-adt "sprites/continueinstruction.jpg" x-correctie 0.5)))

; Highscores menu

(define highscores-menu-list
  (list
   (maak-menu-part-adt "sprites/highscores.jpg" x-correctie 0.01)
   (maak-menu-part-adt "sprites/continueinstruction.jpg" x-correctie 0.9)))