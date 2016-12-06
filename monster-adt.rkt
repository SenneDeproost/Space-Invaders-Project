#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                      -+- MONSTER -+-                     *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(require "config.rkt")

(provide maak-monster-adt)

(define (maak-monster-adt species x-pos y-pos health)
  
  ;+++ POSITION +++;
  (define (set-x! new-x)
    (set! x-pos new-x))
  
  (define (set-y! new-y)
    (set! y-pos new-y))
  
  ;+++ HEALTH +++;
  (define (set-health! n)
    (set! health n))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    ((teken-adt 'teken-monster!) dispatch-monster))
  
  ;+++ VERWIJDER +++;
  (define (verwijder! teken-adt)
    ((teken-adt 'verwijder-monster!) dispatch-monster))
  
  ;+++ MOVE +++;
  (define (move! richting afstand)
    (cond ((eq? richting 'right)
           (set! x-pos (+ x-pos afstand)))
          ((eq? richting 'left)
           (set! x-pos (- x-pos afstand)))
          ((eq? richting 'down)
           (set! y-pos (+ y-pos afstand)))
          (else (display "ERROR: FALSE RICHTING")))) ; Als een niet geldige richting wordt gegeven,is er een error.
  
  ;+++ DESCEND +++;
  (define (descend!)
    (set! y-pos 9))
  
  ;+++ DISPATCH +++;
  (define (dispatch-monster msg)
    (cond
      ((eq? msg 'get-x) x-pos)
      ((eq? msg 'get-y) y-pos)
      
      ((eq? msg 'set-x!) set-x!)
      ((eq? msg 'set-y!) set-y!)
      
      ((eq? msg 'move!) move!)
      
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'verwijder!) verwijder!)
      
      ((eq? msg 'health) health)
      ((eq? msg 'species) species)
      
      ((eq? msg 'set-health!) set-health!)
      
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO MONSTER-ADT"))))
  
  dispatch-monster)