#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                       -+- MENU -+-                       *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(provide maak-menu-adt)

(define (maak-menu-adt)
  
  (define menu-list '())
  
  ;+++ MAP +++;
  (define (voor-alle-menu-parts f)
    (for-each f menu-list))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    (voor-alle-menu-parts (lambda (menu-part-adt) ((menu-part-adt 'teken!) teken-adt))))
  
  ;+++ VOEG TOE +++;
  (define (voeg-menu-part-toe! menu-part-adt teken-adt) 
    (set! menu-list (cons menu-part-adt menu-list))
    ((teken-adt 'voeg-menu-part-toe!) menu-part-adt))
  
  ;+++ VERWIJDER PART +++;
  (define (verwijder-menu-part! menu-part-adt teken-adt)
    (set! menu-list (remove! menu-part-adt menu-list))
    ((teken-adt 'verwijder-menu-part!) menu-part-adt))
  
  ;+++ HULP PROCEDURE +++;
  (define (remove! el list)
    (cond ((null? list) '())
          ((eq? el (car list))
           (cdr list))
          (else (cons (car list)
                      (remove! el (cdr list))))))
  
  ;+++ DISPATCH +++;
  (define (dispatch-menu-list msg)
    (cond
      ((eq? msg 'teken!) teken!)
      
      ((eq? msg 'voor-alle-menu-parts) voor-alle-menu-parts)
      
      ((eq? msg 'voeg-menu-part-toe!) voeg-menu-part-toe!)
      ((eq? msg 'verwijder-menu-part!) verwijder-menu-part!)
      
      ((eq? msg 'menu-part-list) menu-list)
      
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO MENU-LIST-ADT"))))
  
  dispatch-menu-list)
