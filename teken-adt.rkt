#lang racket
;*----------------------------------------------------------*
;*                       //Teken ADT//                      *
;*                      Senne Deproost                      *
;*                           V 3                            *
;*----------------------------------------------------------*

(#%require "graphics.rkt")

(provide maak-teken-adt)

(define (maak-teken-adt titel pix-verticaal pix-horizontaal)
  
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++ CONFIG ++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  (define venster (make-window pix-horizontaal pix-verticaal titel))
  
  ;+++ Achtergrond +++;
  
  ((venster 'set-background!) "black")
  (define background-layer (venster 'make-layer))
  (define background-tiles '())
  
  (define timer-tile (make-tile 100 50))
  (define scorebord-tile (make-tile 200 50))
  (define name-line-tile (make-tile 700 100))
  
  ;+++ Highscores +++;
  
  (define highscore-tiles '())
  (define highscore-layer (venster 'make-layer))
  
  ;+++ Rocket +++;
  
  (define rocket-layer (venster 'make-layer))
  (define rocket-tile '())
  
  ;+++ Bullet +++;
  
  (define bullet-list-layer (venster 'make-layer))
  (define bullet-list-tiles '())
  
  ;+++ Fleet +++;
  
  (define fleet-layer (venster 'make-layer))
  (define fleet-tiles '())
  
  ;+++ Menu +++;
  
  (define menu-list-layer (venster 'make-layer))
  (define menu-list-tiles '())
  
  ;+++ Power up +++;
  
  (define power-up-layer (venster 'make-layer))
  (define power-up-tiles '())
  
  
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++ VOEG TOE +++++++++++++++++++++++++++
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ; Voeg toe procedures worden gebruikt om een element aan het spel toe te voegen.
  ; Aan de tiles-list van het corresponderende ADT wordt een cons-cel toegevoegd met
  ; in de car het ADT en in de cdr de bijhorende tile.
  ; Nadien wordt de tile toegevoegd aan de bijhorende layer en wordt deze getekend.
  
  ; Menu-part ;
  
  (define (voeg-menu-part-toe! menu-part-adt)
    (let ((new-tile (let ((element (menu-part-adt 'element)))
                      (make-bitmap-tile element))))
      
      (set! menu-list-tiles (cons (cons menu-part-adt new-tile) menu-list-tiles))
      ((menu-list-layer 'add-drawable) new-tile)
      (teken! 'menu-part menu-part-adt)))
  
  
  ; Monster ;
  
  (define (voeg-monster-toe! monster-adt)
    (let ((new-tile (let ((species (monster-adt 'species))) ; Er wordt gekeken wat voor soort species het monster-ADT is.
                      (cond ((eq? species 'yellow) (make-bitmap-tile "sprites/monster-yellow.jpg"))
                            ((eq? species 'purple) (make-bitmap-tile "sprites/monster-purple.jpg"))
                            ((eq? species 'green)  (make-bitmap-tile "sprites/monster-green.jpg"))
                            (else (display "ERROR: AN UNKNOWN SPECIES IS GIVEN"))))))
      
      (set! fleet-tiles (cons (cons monster-adt new-tile) fleet-tiles))
      ((fleet-layer 'add-drawable) new-tile)
      (teken! 'monster monster-adt)))
  
  
  ; Bullet ;
  
  (define (voeg-bullet-toe! bullet-adt)
    (let ((new-tile (make-bitmap-tile "sprites/bullet.jpg")))
      
      (set! bullet-list-tiles (cons (cons bullet-adt new-tile) bullet-list-tiles))
      ((bullet-list-layer 'add-drawable) new-tile)
      (teken! 'bullet bullet-adt)))
  
  
  ; Power-up ;
  
  (define (voeg-power-up-toe! power-up-adt)
    (let ((new-tile (let ((species (power-up-adt 'species))) ; Er wordt gekeken wat voor soort species het power-up-ADT is.
                      
                      (cond ((eq? species 'yellow) (make-bitmap-tile "sprites/yellow-power-up.jpg"))
                            ((eq? species 'white)  (make-bitmap-tile "sprites/white-power-up.jpg"))
                            ((eq? species 'red)    (make-bitmap-tile "sprites/red-power-up.jpg"))                           
                            (else (display "ERROR: AN UNKNOWN SPECIES IS GIVEN"))))))
      
      (set! power-up-tiles (cons (cons power-up-adt new-tile) power-up-tiles))
      ((power-up-layer 'add-drawable) new-tile)
      (teken! 'power-up power-up-adt)))
  
  
  ; Rocket ;
  
  (define (voeg-rocket-toe! rocket-adt)
    (let ((tile (make-bitmap-tile "sprites/rocket.jpg")))
      
      (set! rocket-tile (cons (cons rocket-adt tile) rocket-tile))
      ((rocket-layer 'add-drawable) tile)
      (teken! 'rocket rocket-adt)))
  
  
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++ NEEM +++++++++++++++++++++++++++
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ; De neem procedure zal een tile uit de juiste tiles-list halen afhankelijk van het soort ADT dat meegegeven wordt.
  ; De soort is een symbool die als parameter wordt meegegeven. 
  ; De volgorde van de soorten in de conditional is gekozen op basis hoe vaak een bepaald element getekend moet worden.
  (define (neem soort adt)
    (let ((tiles-list (cond
                        ((eq? soort 'rocket)     rocket-tile)
                        ((eq? soort 'monster)    fleet-tiles)
                        ((eq? soort 'bullet)     bullet-list-tiles)
                        ((eq? soort 'power-up)   power-up-tiles)
                        ((eq? soort 'menu-part)  menu-list-tiles)
                        
                        (else "UNKNOWN SOORT FOR NEEM PROCEDURE"))))
      
      (cdr (assoc adt tiles-list))))
  
  
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++ HULP PROCEDURE +++++++++++++++++++++++++++
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ; Hulpprocedure om een element uit een lijst van cons-cellen te verwijderen.
  ; Wordt gebruikt om een tile uit een tileslist te verwijderen.
  
  (define (remove! el list)
    (cond ((null? list) '())
          ((eq? el (caar list)) ; Caar betekent de car eerste cons-cel van de lijst. 
           (cdr list))
          (else (cons (car list)
                      (remove! el (cdr list))))))
  
  
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++ VERWIJDER +++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ; Een element van een bepaalde soort met een bepaald ADT wordt verwijderd.
  
  (define (verwijder! soort adt)
    (let* (
           (tiles/layer        (cond
                                 ((eq? soort 'bullet)    (cons bullet-list-layer     bullet-list-tiles))
                                 ((eq? soort 'monster)   (cons fleet-layer           fleet-tiles))
                                 ((eq? soort 'power-up)  (cons power-up-layer        power-up-tiles))
                                 ((eq? soort 'menu-part) (cons menu-list-layer       menu-list-tiles))
                                 ((eq? soort 'rocket)    (cons rocket-layer          rocket-tile))
                                 (else "UNKNOWN SOORT FOR VERWIJDER PROCEDURE")))
           (layer              (car tiles/layer))
           (tiles-list         (cdr tiles/layer))
           (tile               (neem soort adt)))
      
      ((layer 'remove-drawable) tile)
      (set! tiles-list (remove adt tiles-list))))
  
  
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++ TEKEN +++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ; De teken procedure zal een tile die al op een laag staat op de juiste positie plaatsen.
  
  (define (teken! soort adt)
    (let (
          (adt-x               (* pix-horizontaal (adt 'get-x)))
          (adt-y               (* pix-verticaal   (adt 'get-y)))
          (tile                (neem soort adt)))
      
      ((tile 'set-x!) adt-x)
      ((tile 'set-y!) adt-y)))
  
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++ SPELINFORMATIE +++++++++++++++++++++++++++
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ;+++ Timer +++;
  
  ;Procedure die de y-positie goed zet. Deze verandert niet meer tijdens het spel.
  
  (define (zet-timer-klaar)
    (let ((timer-y (* pix-verticaal 0.95)))
      ((timer-tile 'set-y!) timer-y)))
  
  ; Voert de bovenste procedure uit bij aanmaak.
  
  (zet-timer-klaar)
  
  ; Procedure die de timer zal zetten op zijn juiste x-positie.
  ; Bij elke oproep zal de tile leeggemaakt worden en wordt de nieuwe tijd getekend.
  (define (teken-timer! tijd)
    (let* 
        ((tijdstring               (number->string tijd))
         (textstring               (string-append "TIME: " tijdstring))
         (length                   (/ (string-length textstring) pix-horizontaal))
         (timer-x                  0))
      
      (timer-tile 'clear)
      ((timer-tile 'set-x!) timer-x)
      ((timer-tile 'draw-text) textstring 15 0 0 "white")
      ((background-layer 'add-drawable) timer-tile)))
  
  ; Verwijder de tile uit de laag.
  
  (define (verwijder-timer!)
    (timer-tile 'clear)
    ((background-layer 'remove-drawable) timer-tile))
  
  
  ;+++ Scorebord +++;
  
  ; Procedure die de y-positie goed zet. Deze verandert niet meer tijdens het spel.
  
  (define (zet-scorebord-klaar)
    (let ((score-y (* pix-verticaal 0.95)))
      ((scorebord-tile 'set-y!) score-y)))
  
  ; Voert de bovenste procedure uit bij aanmaak.
  
  (zet-scorebord-klaar)
  
  ; Procedure die het scorebord zal zetten op zijn juiste x-positie.
  ; Bij elke oproep zal de tile leeggemaakt worden en wordt de nieuwe score getekend.
  
  (define (teken-scorebord! score)
    (let* 
        ((scorestring               (number->string score))
         (textstring                (string-append "SCORE: " scorestring))
         (length                    (/ (string-length textstring) pix-horizontaal))
         (score-x                   (* pix-horizontaal (- 1 (* length 12)))))
      
      (scorebord-tile 'clear)
      ((scorebord-tile 'set-x!) score-x)
      ((scorebord-tile 'draw-text) textstring 15 0 0 "white")
      ((background-layer 'add-drawable) scorebord-tile)))
  
  ; Procedure om scorebord te verwijderen.
  
  (define (verwijder-scorebord!)
    (scorebord-tile 'clear)
    ((background-layer 'remove-drawable) scorebord-tile))
  
  
  ;+++ Name-line +++;
  
  ; Procedure die de y-positie goed zet. Deze verandert niet meer tijdens het spel.
  
  (define (zet-name-line-klaar)
    (let ((y (* pix-verticaal 0.45)))
      ((name-line-tile 'set-y!) y)))
  
  ; Voert de bovenste procedure uit.
  
  (zet-name-line-klaar)
  
  ; Procedure die de nameline zal zetten op zijn juiste x-positie.
  ; Bij elke oproep zal de tile leeggemaakt worden en wordt de nieuwe naam getekend.
  
  (define (teken-name-line! name)
    (let* ((length                   (/ (string-length name) pix-horizontaal))
           (x                        (* pix-horizontaal (- 0.5 (* 6.5 length)))))
      
      (name-line-tile 'clear)
      ((name-line-tile 'set-x!) x)
      ((name-line-tile 'draw-text) name 20 0 0 "white")
      ((background-layer 'add-drawable) name-line-tile)))
  
  ; Procedure om nameline te verwijderen.
  
  (define (verwijder-name-line!)
    (name-line-tile 'clear)
    ((background-layer 'remove-drawable) name-line-tile))
  
  
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;+++++++++++++++++++++++++ HIGHSCORES +++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ; Maak een lijst van tiles aan.
  
  (define (maak-highscore-tiles-list aantal)
    (define (iter counter result-list)
      (if (= counter 0)
          result-list
          (iter (- counter 1) (cons (make-tile 700 50) result-list))))
    (iter aantal '()))
  
  ;Procedure die de y-positie goed zet. Deze verandert niet meer tijdens het spel. 
  
  (define (zet-y-juist highscore-tile tile-y)
    (let ((y (* pix-verticaal tile-y)))
      ((highscore-tile 'set-y!) y)))
  
  ; Zet de y-positie van alle tiles goed.
  
  (define (zet-highscore-tiles-klaar highscore-tiles-list)
    (define (iter counter list)
      (cond ((null? list)
             highscore-tiles-list)
            (else
             (zet-y-juist (car list) counter) ; Zet de y-waarde juist.
             (iter (+ counter 0.1) (cdr list)))))
    (iter 0.3 highscore-tiles-list))
  
  ; Voeg highscores toe aan highscore-layer.
  
  (define (voeg-highscores-toe!)
    (set! highscore-tiles (zet-highscore-tiles-klaar (maak-highscore-tiles-list 5)))
    (for-each (lambda (tile) ((highscore-layer 'add-drawable) tile)) highscore-tiles))

  ; Teken de highscores.
  
  (define (teken-highscores! vector)
    (let* ((x                        (* 0.3 pix-horizontaal)))
      
      (voeg-highscores-toe!)
      
      (for-each (lambda (tile) ((tile 'set-x!) x)) highscore-tiles) ; Zet de x-waarde van alle tiles goed.

; Zet een entry vector om naar een string die als tekst kan geschreven worden op een tile.
      
      (define (entry->string entry)
        (if (eq? entry 'empty)
            (cons " " " ")
            (cons ; Voeg naam-string en score-string samen.
             (symbol->string (vector-ref entry 0)) ; Converteer de naam naar een string.
             (number->string (vector-ref entry 1))))) ; Converteer de score naar een string.
      
      ; Procedure die de tekst zal tekenen op de tiles
      
      (define (draw-text tiles-list entry-vector)
        (define (iter tiles-list vector counter)
          (cond ((not (= counter 5))
                 
                 ; Teken de naam
                 (((car tiles-list) 'draw-text)
                  (car (entry->string (vector-ref vector counter))) ; Data voor de tekst.
                  20
                  0
                  0
                  "white")
                 
                 ; Teken de score
                 (((car tiles-list) 'draw-text)
                  (cdr (entry->string (vector-ref vector counter))) ; Data voor de tekst.
                  20
                  200
                  0
                  "white")
                 
                 
                 (iter  (cdr tiles-list ) vector  (+ counter 1)))))
        (iter tiles-list entry-vector 0))
      
      (draw-text highscore-tiles vector) ; Oproep van de bovenste procedure.
      
      (for-each (lambda (tile) ((highscore-layer 'add-drawable) tile)) highscore-tiles))) ; Voeg alle tiles toe aan de laag.
  
  ; Procedure om highscores te verwijderen van het scherm.
  
  (define (verwijder-highscores!)
    (for-each (lambda (tile) (tile 'clear)) highscore-tiles)
    (for-each (lambda (tile) ((highscore-layer 'remove-drawable) tile)) highscore-tiles)
    (set! highscore-tiles '()))
  
  
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;++++++++++++++++++++++++ CALLBACKS ++++++++++++++++++++++++++
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  (define (set-spel-lus-functie! functie)
    ((venster 'set-update-callback!) functie))
  
  (define (set-toets-functie! functie)
    ((venster 'set-key-callback!) functie))
  
  
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;+++++++++++++++++++++++++ DISPATCH +++++++++++++++++++++++++
  ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  (define (dispatch msg)
    (cond
      ((eq? msg 'teken-menu-part!) (lambda (adt) (teken! 'menu-part  adt)))
      ((eq? msg 'teken-rocket!)    (lambda (adt) (teken! 'rocket     adt)))
      ((eq? msg 'teken-bullet!)    (lambda (adt) (teken! 'bullet     adt)))
      ((eq? msg 'teken-monster!)   (lambda (adt) (teken! 'monster    adt)))
      ((eq? msg 'teken-power-up!)  (lambda (adt) (teken! 'power-up   adt)))
      
      ((eq? msg 'voeg-menu-part-toe!) voeg-menu-part-toe!)
      ((eq? msg 'voeg-bullet-toe!)    voeg-bullet-toe!)
      ((eq? msg 'voeg-monster-toe!)   voeg-monster-toe!)
      ((eq? msg 'voeg-power-up-toe!)  voeg-power-up-toe!)
      ((eq? msg 'voeg-rocket-toe!)    voeg-rocket-toe!)
      
      ((eq? msg 'verwijder-rocket!)    (lambda (adt) (verwijder! 'rocket    adt)))
      ((eq? msg 'verwijder-power-up!)  (lambda (adt) (verwijder! 'power-up  adt)))
      ((eq? msg 'verwijder-menu-part!) (lambda (adt) (verwijder! 'menu-part adt)))
      ((eq? msg 'verwijder-bullet!)    (lambda (adt) (verwijder! 'bullet    adt)))
      ((eq? msg 'verwijder-monster!)   (lambda (adt) (verwijder! 'monster   adt)))
      
      ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
      ((eq? msg 'set-toets-functie!)    set-toets-functie!)
      
      ((eq? msg 'teken-timer!)      teken-timer!)
      ((eq? msg 'teken-scorebord!)  teken-scorebord!)
      ((eq? msg 'teken-name-line!)  teken-name-line!)
      ((eq? msg 'teken-highscores!) teken-highscores!)
      
      ((eq? msg 'verwijder-timer!)      (verwijder-timer!))
      ((eq? msg 'verwijder-scorebord!)  (verwijder-scorebord!))
      ((eq? msg 'verwijder-name-line!)  (verwijder-name-line!))
      ((eq? msg 'verwijder-highscores!) (verwijder-highscores!))
      
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO TEKEN-ADT"))))
  
  dispatch)