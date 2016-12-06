#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                   -+- BULLET-LIST -+-                    *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(provide maak-bullet-list-adt)

(define (maak-bullet-list-adt)
  
  (define bullet-list '())
  
  ;+++ MAP +++;
  (define (voor-alle-bullets f)
    (for-each f bullet-list))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    (voor-alle-bullets (lambda (bullet-adt) ((bullet-adt 'teken!) teken-adt))))
  
  ;+++ VOEG BULLET TOE +++;
  (define (voeg-bullet-toe! bullet-adt teken-adt) 
    (set! bullet-list (cons bullet-adt bullet-list))
    ((teken-adt 'voeg-bullet-toe!) bullet-adt))
  
  ;+++ VERWIJDER BULLET +++;
  (define (verwijder-bullet! bullet-adt teken-adt)
    (set! bullet-list (remove! bullet-adt bullet-list))
    ((bullet-adt 'verwijder!) teken-adt))
  
  ;+++ HULP PROCEDURE +++;
  (define (remove! el list)
    (cond ((null? list) '())
          ((eq? el (car list))
           (cdr list))
          (else (cons (car list)
                      (remove! el (cdr list))))))
  
  ;+++ DISPATCH +++;
  (define (dispatch-bullet-list msg)
    (cond
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'voor-alle-bullets) voor-alle-bullets)
      ((eq? msg 'voeg-bullet-toe!) voeg-bullet-toe!)
      ((eq? msg 'verwijder-bullet!) verwijder-bullet!)
      
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO BULLET-LIST-ADT"))))
  
  dispatch-bullet-list)