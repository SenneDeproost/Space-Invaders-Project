#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                       -+- FLEET -+-                      *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(provide maak-fleet-adt)

(define (maak-fleet-adt)
  
  (define fleet '())
  
  ;+++ MAP +++;
  (define (voor-alle-monsters f)
    (map f fleet))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    (voor-alle-monsters (lambda (monster-adt)
                          ((monster-adt 'teken!) teken-adt))))
  
  ;+++ VOEG MONSTER TOE +++;
  (define (voeg-monster-toe! monster-adt teken-adt)
    (set! fleet (cons monster-adt fleet))
    ((teken-adt 'voeg-monster-toe!) monster-adt))
  
  ;+++ VERWIJDER MONSTER +++;
  (define (verwijder-monster! monster-adt teken-adt)
    (set! fleet (remove! monster-adt fleet))
    ((monster-adt 'verwijder!) teken-adt))
  
  ;+++ HULP PROCEDURE +++;
  (define (remove! el list)
    (cond ((null? list) '())
          ((eq? el (car list))
           (cdr list))
          (else (cons (car list)
                      (remove! el (cdr list))))))
  
  ;+++ MOVE +++;
  (define (move! richting afstand)
    (voor-alle-monsters (lambda (monster-adt) ((monster-adt 'move!) richting afstand))))
  
  ;+++ DISPATCH +++;
  (define (dispatch-fleet msg)
    (cond
      ((eq? msg 'teken!) teken!)
      
      ((eq? msg 'voor-alle-monsters) voor-alle-monsters)
      
      ((eq? msg 'voeg-monster-toe!) voeg-monster-toe!)
      ((eq? msg 'verwijder-monster!) verwijder-monster!)
      
      ((eq? msg 'move!) move!)
      
      ((eq? msg 'fleet) fleet)
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO FLEET-ADT"))))
  
  dispatch-fleet)