#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                    -+- SCOREBORD -+-                     *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(provide maak-scorebord-adt)

(define (maak-scorebord-adt)
  
  (define score 0)
  
  ;+++ VERHOOG +++;
  (define (verhoog-met! n)
    (set! score (+ score n)))
  
  ;+++ RESET +++;
  (define (reset!)
    (set! score 0))
  
  ;+++ TEKEN +++;
  (define (teken! teken-adt)
    ((teken-adt 'teken-scorebord!) score))

  ;+++ DISPATCH +++;
  (define (dispatch-scorebord msg)
    (cond
      ((eq? msg 'verhoog-met!) verhoog-met!)
      ((eq? msg 'reset!) (reset!)) 
      
      ((eq? msg 'get-score) score)
      
      ((eq? msg 'teken!) teken!)

      
      (else (display "ERROR: ")
            (display msg)
            (display " IS AN ILLEGAL REQUEST TO SCOREBORD-ADT"))))
  
  dispatch-scorebord)