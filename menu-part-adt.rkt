#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                    -+- MENU-PART -+-                     *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(provide maak-menu-part-adt)

(define (maak-menu-part-adt element x-pos y-pos)
  
  ;+++ SET POSITION +++;
  (define (set-x! new-pos)
    (set! x-pos new-pos))
  
  (define (set-y! new-pos)
    (set! y-pos new-pos))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    ((teken-adt 'teken-menu-part!) dispatch-menu-part))
  
  ;+++ VERWIJDER +++;
  (define (verwijder! teken-adt)
    (teken-adt 'verwijder-menu-part!))
  
  ;+++ DISPATCH +++;
  (define (dispatch-menu-part msg)
    (cond
      ((eq? msg 'get-x) x-pos)
      ((eq? msg 'get-y) y-pos)
      
      ((eq? msg 'set-x!) set-x!)
      ((eq? msg 'set-y!) set-y!)
      
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'verwijder!) verwijder!)
      
      ((eq? msg 'element) element)
      
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO MENU-PART-ADT"))))
  
  dispatch-menu-part)