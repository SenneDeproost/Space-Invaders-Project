#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                      -+- ROCKET -+-                      *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(provide maak-rocket-adt)

(require "config.rkt")

(define (maak-rocket-adt)
  
  ;+++ LOCALS +++;
  (define x-pos 0.5)
  (define y-pos 0.80)
  
  ;+++ MOVE +++;
  (define (move! richting)
    (cond ((eq? richting 'rechts)
           (set! x-pos (+ x-pos rocket-speed)))
          ((eq? richting 'links)
           (set! x-pos (- x-pos rocket-speed)))))
  
  ;+++ SET +++;
  (define (set-x! new-x)
    (set! x-pos new-x))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    ((teken-adt 'teken-rocket!) dispatch-rocket))
  
  ;+++ VERDWIJDER +++;
  (define (verwijder! teken-adt)
    ((teken-adt 'verwijder-rocket!) dispatch-rocket))
  
  ;+++ DISPATCH +++;
  (define (dispatch-rocket msg)
    (cond
      ((eq? msg 'get-x) x-pos)
      ((eq? msg 'get-y) y-pos)

      ((eq? msg 'set-x!) set-x!)
      
      ((eq? msg 'move!) move!)
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'verwijder!) verwijder!)
      
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO ROCKET-ADT"))))
  
  dispatch-rocket)