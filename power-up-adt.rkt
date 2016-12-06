#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                     -+- POWER-UP -+-                     *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(provide maak-power-up-adt)

(define (maak-power-up-adt species x-pos y-pos creation-time)
  
  ;+++ POSITION +++;
  (define (set-x! new-x)
    (set! x-pos new-x))
  
  (define (set-y! new-y)
    (set! y-pos new-y))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    ((teken-adt 'voeg-power-up-toe!) dispatch-power-up)
    ((teken-adt 'teken-power-up!) dispatch-power-up))
  
  ;+++ VERWIJDER +++;
  (define (verwijder! teken-adt)
    ((teken-adt 'verwijder-power-up!) dispatch-power-up))
  
  ;+++ DISPATCH +++;
  (define (dispatch-power-up msg)
    (cond
      ((eq? msg 'get-x) x-pos)
      ((eq? msg 'get-y) y-pos)
      
      ((eq? msg 'set-x!) set-x!)
      ((eq? msg 'set-y!) set-y!)
      
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'verwijder!) verwijder!)
      
      ((eq? msg 'creation-time) creation-time)      
      ((eq? msg 'species) species)
      
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO POWER-UP-ADT"))))
  
  dispatch-power-up)