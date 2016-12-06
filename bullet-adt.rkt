#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                      -+- BULLET -+-                      *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(require "config.rkt")

(provide maak-bullet-adt)

(define (maak-bullet-adt)
  
  ;+++ LOCALS +++;
  (define x-pos 0) ;Waarden worden gecorigeerd door het spel-adt bij aanmaak
  (define y-pos 0)
  
  ;+++ MOVE +++;
  (define (move!)
    (set! y-pos (- y-pos bullet-speed)))
  
  ;+++ SET POSITION +++;
  (define (set-x! new-pos)
    (set! x-pos new-pos))
  
  (define (set-y! new-pos)
    (set! y-pos new-pos))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    ((teken-adt 'teken-bullet!) dispatch-bullet))
  
  ;+++ VERWIJDER +++;
  (define (verwijder! teken-adt)
    ((teken-adt 'verwijder-bullet!) dispatch-bullet))
  
  ;+++ DISPATCH +++;
  (define (dispatch-bullet msg)
    (cond
      ((eq? msg 'get-x) x-pos)
      ((eq? msg 'get-y) y-pos)
      
      ((eq? msg 'set-x!) set-x!)
      ((eq? msg 'set-y!) set-y!)
      
      ((eq? msg 'move!) (move!))
      
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'verwijder!) verwijder!)
      
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO BULLET-ADT"))))
  
  dispatch-bullet)