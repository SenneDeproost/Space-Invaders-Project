#lang racket
;*----------------------------------------------------------*
;*                    //Space Invaders//                    *
;*                                                          *
;*                       -+- SPEL -+-                       *
;*                                                          *
;*                      Senne Deproost                      *
;*----------------------------------------------------------*

(require "config.rkt")
(require "menus.rkt")
(require "menu-adt.rkt")
(require "teken-adt.rkt")
(require "rocket-adt.rkt")
(require "bullet-adt.rkt")
(require "bullet-list-adt.rkt")
(require "fleet-adt.rkt")
(require "scorebord-adt.rkt")
(require "power-up-adt.rkt")
(require "monster-adt.rkt")
(require "datasave.rkt")

(provide maak-spel-adt)

(define (maak-spel-adt)
  
  ; Houdt speltijd bij
  
  (define speltijd 0)
  
  ; Variabele, nodig voor naar beneden te gaan van de fleet.
  
  (define down? #t)
  
  ; Variabele die het huidige level bijhoudt.
  
  (define level 1)
  
  ; Score van de speler.
  
  (define score 0)
  
  ; Definitie van het scorebord-ADT
  
  (define scorebord-adt                 (maak-scorebord-adt))
  
  
  (define teken-adt (maak-teken-adt titel venster-hoogte venster-breedte))
  
  
  ;---------------------***************------------***************------------------
  ;---------------------*************** START MENU ***************------------------
  ;---------------------***************------------***************------------------
  
  (define (start-menu)
    
    (define menu-adt (maak-menu-adt)) ; Maak een menu-ADT aan.
    
    ;----------------------------------------------
    ;------------------ TOETSFUNCTIE --------------
    ;----------------------------------------------
    
    ; Simpele toetsfunctie controleert wanneer op een bepaalde toets wordt gedrukt om het spel te starten.
    
    (define (toets-functie toets)
      (cond ((eq? toets '#\return)
             
             ; Verwijder alle onderdelen van het menu.
             
             ((menu-adt 'voor-alle-menu-parts) (lambda (menu-part-adt) ((menu-adt 'verwijder-menu-part!) menu-part-adt teken-adt)))            
             
             ; Start het spel.
             
             (start))
            
            ; Als de toets H wordt ingedrukt wordt er gegaan naar de highscores.
            
            ((eq? toets '#\h)
             
             ; Verwijder alle onderdelen van het menu.
             
             ((menu-adt 'voor-alle-menu-parts) (lambda (menu-part-adt) ((menu-adt 'verwijder-menu-part!) menu-part-adt teken-adt)))
             
             (highscores-menu))))
    
    ; Callback.
    
    ((teken-adt 'set-toets-functie!) toets-functie)
    
    ;----------------------------------------------
    ;----------------- VOORBEREIDING --------------
    ;----------------------------------------------
    
    ;Procedure die de volledige fleet-lijst meegeeft aan het fleet-ADT in het begin.
    
    (map (lambda (menu-part-adt) ((menu-adt 'voeg-menu-part-toe!) menu-part-adt teken-adt)) start-menu-list)
    ((menu-adt 'teken!) teken-adt))
  
  ;---------------------***************----------------***************------------------
  ;---------------------*************** GAME OVER MENU ***************------------------
  ;---------------------***************----------------***************------------------
  
  (define (gameover-menu)
    
    (define menu-adt (maak-menu-adt))
    
    ;----------------------------------------------
    ;------------------ TOETSFUNCTIE --------------
    ;----------------------------------------------
    
    ; Simpele toetsfunctie controleert wanneer op een bepaalde toets wordt gedrukt om het spel te herstarten.
    
    (define (toets-functie toets)
      (cond
        ((eq? toets '#\r)
         
         ; Verwijder alle onderdelen van het menu.
         
         ((menu-adt 'voor-alle-menu-parts) (lambda (menu-part-adt) ((menu-adt 'verwijder-menu-part!) menu-part-adt teken-adt)))
         
         ; Ga naar highscore-insert menu.
         
         (highscore-insert-menu))))
    
    ; Callbacks. Een lege lambda wordt de nieuwe spellusfunctie.
    
    ((teken-adt 'set-toets-functie!) toets-functie)
    ((teken-adt 'set-spel-lus-functie!) (lambda (x) (void)))
    
    
    ;----------------------------------------------
    ;----------------- VOORBEREIDING --------------
    ;----------------------------------------------
    
    ; Klaarzetten van de menu-parts.
    
    (map (lambda (menu-part-adt) ((menu-adt 'voeg-menu-part-toe!) menu-part-adt teken-adt)) gameover-menu-list)
    ((menu-adt 'teken!) teken-adt))
  
  ;---------------------***************-----------------***************------------------
  ;---------------------*************** HIGHSCORES MENU ***************------------------
  ;---------------------***************-----------------***************------------------
  
  (define (highscores-menu)
    
    (define menu-adt (maak-menu-adt))
    
    ;----------------------------------------------
    ;------------------ TOETSFUNCTIE --------------
    ;----------------------------------------------
    
    ; Simpele toetsfunctie controleert wanneer op een bepaalde toets wordt gedrukt om het spel te herstarten.
    
    (define (toets-functie toets)
      (cond
        ((eq? toets '#\return)
         
         ; Verwijder alle onderdelen van het menu.
         
         ((menu-adt 'voor-alle-menu-parts) (lambda (menu-part-adt) ((menu-adt 'verwijder-menu-part!) menu-part-adt teken-adt)))
         
         ; Verwijder de highscores
         
         (teken-adt 'verwijder-highscores!)
         
         ; Ga naar start menu.
         
         (start-menu))))
    
    
    (teken-adt 'teken-highscores!)
    ((teken-adt 'set-toets-functie!) toets-functie)
    ((teken-adt 'set-spel-lus-functie!) (lambda (x) (void)))
    
    
    ;----------------------------------------------
    ;----------------- VOORBEREIDING --------------
    ;----------------------------------------------
    
    ; Klaarzetten van de menu-parts.
    
    (map (lambda (menu-part-adt) ((menu-adt 'voeg-menu-part-toe!) menu-part-adt teken-adt)) highscores-menu-list)
    ((menu-adt 'teken!) teken-adt)
    
    ; Teken de highscores
    
    ((teken-adt 'teken-highscores!) save-file))
  
  ;---------------------***************-----------------------***************------------------
  ;---------------------*************** HIGHSCORE INSERT MENU ***************------------------
  ;---------------------***************-----------------------***************------------------
  
  (define (highscore-insert-menu)
    
    (define menu-adt (maak-menu-adt))
    
    ;------------------------------------------------
    ;------------------ HULPPROCEDURES --------------
    ;------------------------------------------------
    
    (define name-buffer "") ; Hiernaar worden de letters van de naam van de speler geschreven vooraleer het wordt gebonden aan player-name
    
    
    ;----------------------------------------------
    ;------------------ TOETSFUNCTIE --------------
    ;----------------------------------------------
    
    ; Simpele toetsfunctie controleert wanneer op een bepaalde toets wordt gedrukt om het spel te starten.
    
    (define (toets-functie toets)
      (cond
        ((eq? toets '#\return)
         
         ; Verwijder de name-line.
         
         (teken-adt 'verwijder-name-line!)
         
         ; Verwijder alle onderdelen van het menu.
         
         ((menu-adt 'voor-alle-menu-parts) (lambda (menu-part-adt) ((menu-adt 'verwijder-menu-part!) menu-part-adt teken-adt)))
         
         ; Geef de naam en de score van de speler mee aan datasave om in de highscores te plaatsen.
         
         (update-highscores! (string->symbol name-buffer) score) 
         
         ; Er wordt naar de highscores gegaan.
         
         (highscores-menu))
        
        ; Als de toets wordt gereleased, r-shift of spatie is.
        
        ((or (eq? toets 'release) (eq? toets 'rshift) (eq? toets '#\space))
         (lambda (x) (void)))
        
        ; Als de backspace wordt ingedrukt kan men een karakter verwijderen.
        
        ((eq? toets '#\backspace)
         (when (> (string-length name-buffer) 0) ; Er kunnen enkel karakters verwijderd worden als er minstens één karakter op de name-line staat
           (set! name-buffer (substring name-buffer 0 (- (string-length name-buffer) 1))) ; Werk de name-buffer bij
           ((teken-adt 'teken-name-line!) name-buffer))) ; Neem de substring die enkel de laatste letter van de string niet bevat.
        
        ; De karakters van de naam van de speler worden naar name-buffer geschreven.
        
        (else
         (set! name-buffer (string-append name-buffer (make-string 1 toets)))
         
         ; Teken de name-buffer op het scherm
         
         ((teken-adt 'teken-name-line!) name-buffer)))) ; De naam wordt langer naarmate er meer toetsen in worden geduwt na elkaar.
    
    ; Callbacks.
    
    ((teken-adt 'set-toets-functie!) toets-functie)
    ((teken-adt 'set-spel-lus-functie!) (lambda (x) (void)))
    
    
    
    ;----------------------------------------------
    ;----------------- VOORBEREIDING --------------
    ;----------------------------------------------
    
    ;Procedure die de volledige fleet-lijst meegeeft aan het fleet-ADT in het begin.
    
    (map (lambda (menu-part-adt) ((menu-adt 'voeg-menu-part-toe!) menu-part-adt teken-adt)) highscore-insert-menu-list)
    ((menu-adt 'teken!) teken-adt))
  
  ;---------------------***************---------------------***************------------------
  ;---------------------*************** LEVEL COMPLETE MENU ***************------------------
  ;---------------------***************---------------------***************------------------
  
  (define (levelcomplete-menu)
    
    (define menu-adt (maak-menu-adt))
    
    ;----------------------------------------------
    ;------------------ TOETSFUNCTIE --------------
    ;----------------------------------------------
    
    ; Simpele toetsfunctie controleert wanneer op een bepaalde toets wordt gedrukt om het spel te starten.
    
    (define (toets-functie toets)
      (cond ((eq? toets '#\return)
             
             ; Verwijder alle onderdelen van het menu.
             
             ((menu-adt 'voor-alle-menu-parts) (lambda (menu-part-adt) ((menu-adt 'verwijder-menu-part!) menu-part-adt teken-adt)))
             
             ; Er wordt naar het start menu gegaan.
             
             (start))))
    
    ; Callbacks
    
    ((teken-adt 'set-toets-functie!) toets-functie)
    ((teken-adt 'set-spel-lus-functie!) (lambda (x) (void)))
    
    
    ;----------------------------------------------
    ;----------------- VOORBEREIDING --------------
    ;----------------------------------------------
    
    ;Procedure die de volledige fleet-lijst meegeeft aan het fleet-ADT in het begin.
    
    (map (lambda (menu-part-adt) ((menu-adt 'voeg-menu-part-toe!) menu-part-adt teken-adt)) levelcomplete-menu-list)
    ((menu-adt 'teken!) teken-adt))
  
  
  
  ;---------------------***************-------***************------------------
  ;---------------------*************** START ***************------------------
  ;---------------------***************-------***************------------------ 
  
  (define (start)
    
    (define rocket-adt                    (maak-rocket-adt))
    (define bullet-list-adt               (maak-bullet-list-adt))
    (define fleet-adt                     (maak-fleet-adt))
    
    ; Voeg een rocket toe aan het spel en teken deze.
    ((teken-adt 'voeg-rocket-toe!) rocket-adt)
    (rocket-adt 'teken!)
    
    ;----------------------------------------------
    ;------------------ VARIABELEN ----------------
    ;----------------------------------------------
    
    ; Variabelen om de tijd van een power-up bij te houden
    
    (define yellow-power-up-time 0)
    (define white-power-up-time 0)
    (define red-power-up-time 0)
    
    ; Variabelen uit CONFIG die door de power-ups aangepast kunnen worden.
    
    (define vertical-speed-fleet 0)
    (set! vertical-speed-fleet vertical-speed-monster)
    (define bullet-impact 1) ; Schade die de kogels aanrichten bij een monstertje.
    (define fleet-refresh fleet-refresh-rate) ; Refreshrate die verhoogt bij elk level.
    
    ; Een list moet bijhouden welke powerups op het scherm staan. Bij elke power-up zit er ook bij hoelang het nog op het schem mag blijven staan.
    
    (define power-list '())
    
    ; Geeft de oudste power-up terug in de power-list.
    
    (define (serve-oldest-powerup!) (car power-list) (set! power-list (cdr power-list)))
    
    ; Procedure om een element uit een lijst verwijderen.
    
    (define (remove! el list)
      (cond ((null? list) '())
            ((eq? el (car list))
             (cdr list))
            (else (cons (car list)
                        (remove! el (cdr list))))))
    
    ;----------------------------------------------
    ;---------------- PAUZE MENU --------------
    ;----------------------------------------------  
    
    (define (pauze-menu)
      
      (define menu-adt (maak-menu-adt))
      
      
      ;----------------------------------------------
      ;------------------ TOETSFUNCTIE --------------
      ;----------------------------------------------
      
      ; Simpele toetsfunctie controleert wanneer op een bepaalde toets wordt gedrukt om het spel te starten.
      
      (define (toets-functie toets)
        (cond ((or (eq? toets 'pause) (eq? toets '#\p))
               
               ; Verwijder alle onderdelen van het menu.
               
               ((menu-adt 'voor-alle-menu-parts) (lambda (menu-part-adt) ((menu-adt 'verwijder-menu-part!) menu-part-adt teken-adt)))
               
               ; Zet de spel-lus terug in gang en zet toets functie terug in orde.
               
               ((teken-adt 'set-spel-lus-functie!) spel-lus-functie)
               ((teken-adt 'set-toets-functie!) game-toets-functie))))
      
      ;Callbacks
      
      ((teken-adt 'set-toets-functie!) toets-functie)
      ((teken-adt 'set-spel-lus-functie!) (lambda (x) (void)))
      
      ;----------------------------------------------
      ;----------------- VOORBEREIDING --------------
      ;----------------------------------------------
      
      ;Procedure die de volledige fleet-lijst meegeeft aan het fleet-ADT in het begin.
      
      (map (lambda (menu-part-adt) ((menu-adt 'voeg-menu-part-toe!) menu-part-adt teken-adt)) pauze-menu-list)
      ((menu-adt 'teken!) teken-adt))
    
    
    ;----------------------------------------------
    ;---------------- HULPPROCEDURES --------------
    ;----------------------------------------------
    
    ; Bullet-tijd, power-up-tijd en fleet-tijd worden in het begin op nul gezet.
    
    (define bullet-tijd 0)
    (define fleet-tijd 0)
    (define power-up-tijd 0)
    (define tijd-parameter 0) ; Wordt gebruikt voor de speltijd-loop
    
    ;/////////////////;
    ;+++ COLLISION +++;
    ;/////////////////;
    
    ; Test om te bepalen of een bullet een monster heeft geraakt.
    
    (define (hit-monster bullet-adt monster-adt)
      (let* ((tile-w                 (correction 30 venster-breedte)) ; Nodig om absoluut naar relatief om te zetten. Tegelbreedte wordt op voorhand bepaald.
             (tile-h                 (correction 30 venster-hoogte)) ; Tegelhoogte wordt op voorhand bepaald.  
             (bullet-x               (bullet-adt 'get-x))
             (bullet-y               (bullet-adt 'get-y))
             (a-x                    (monster-adt 'get-x))
             (b-x                    (+ a-x tile-w))
             (a-y                    (monster-adt 'get-y))
             (b-y                    (+ a-y tile-h))
             (species                (monster-adt 'species))
             (monster-value          (cdr (assoc species (list (cons 'yellow 50) (cons 'purple 100) (cons 'green 150))))) ; Zoekt de score op die bij het monster hoort
             (health                 (monster-adt 'health)))
        
        ; Er wordt gekeken of de coördinaat van de bullet in het interval ligt van de dimensies van de monster-tile.
        
        (when (and                                      
               (>= bullet-x a-x) (<= bullet-x b-x)
               (>= bullet-y a-y) (<= bullet-y b-y))
          
          ; Breng schade toe aan monster. Als het monster maar 1 als health heeft, wordt hij van het scherm verwjiderd.
          
          (if (<= health 1)
              
              (begin
                ((fleet-adt 'verwijder-monster!) monster-adt teken-adt)
                ((scorebord-adt 'verhoog-met!) monster-value) ; Scorebord wordt met een bepaald aantal verhoogt.
                ((scorebord-adt 'teken!) teken-adt))
              
              ; Anders wordt er van de health afgetrokken.
              
              ((monster-adt 'set-health!) (- health 1)))
          
          ; Verwijder de kogel van het scherm.
          
          ((bullet-list-adt 'verwijder-bullet!) bullet-adt teken-adt))))
    
    ; Procedure die gemapt zal worden op de hele fleet.
    
    (define (hit-detection bullet-adt fleet-adt) ((fleet-adt 'voor-alle-monsters) (lambda (monster-adt) (hit-monster bullet-adt monster-adt))))
    
    ; Controle of er een botsing is tussen de rocket en een power-up.
    ; Deze procedure werkt met intervalen, net zoals de hit-monster procedure.
    
    (define (hit-power-up rocket-adt power-up-adt)
      (let*  ((rocket-a-x               (rocket-adt 'get-x))
              (rocket-b-x               (+ rocket-a-x (correction 30 venster-breedte)))
              (power-up-a-x             (power-up-adt 'get-x))
              (power-up-b-x             (+ power-up-a-x (correction 15 venster-breedte))))
        
        ; Als de rocket zich niet buiten het interval van x-waarden van de power-up bevindt, dan heeft deze de power-up aangeraakt.
        
        (and (not (and (< rocket-a-x power-up-a-x) (< rocket-b-x power-up-a-x)))
             (not (and (> rocket-a-x power-up-b-x) (> rocket-b-x power-up-b-x))))))
    
    
    ;//////////////;
    ;+++ BULLET +++;
    ;//////////////;  
    
    ; Wanneer control wordt ingedrukt, wordt er een nieuwe bullet aangemaakt en toegevoegd aan bullet-list-adt.
    
    (define (create-bullet!)
      (let* ((bullet-adt               (maak-bullet-adt))
             (x                        (+ (rocket-adt 'get-x) (correction 15 venster-breedte))) ; Correctie om ervoor te zorgen dat de kogel in het midden van de rocket wordt gelanceerd.
             (y                        (rocket-adt 'get-y))
             (x!                       (bullet-adt 'set-x!))
             (y!                       (bullet-adt 'set-y!)))
        ((bullet-list-adt 'voeg-bullet-toe!) bullet-adt teken-adt) ; Bullet wordt toegevoegd aan bullet-list-adt.
        (x! x) ; Coördinaten van bullet-adt worden op de waarden van centrum rocket gezet
        (y! y)))
    
    
    
    ;/////////////;
    ;+++ FLEET +++;
    ;/////////////;
    
    ; Deze variabele houdt bij in welke richting de fleet moet bewegen.
    
    (define fleet-richting 'left)
    
    ; Test om te kijken of een monster-adt van de fleet de rand van het venter heeft bereikt of de ondergrens.
    
    (define (fleet-te-ver? fleet-list)
      (let* ((onderkant                0.75)
             (linkerrand               0.02)
             (rechterrand              (correction (- venster-breedte 60) venster-breedte)))
        (cond
          ((null? fleet-list) #f)
          ((or (<= ((car fleet-list) 'get-x) linkerrand) (>= ((car fleet-list) 'get-x) rechterrand)) #t)
          ((>= ((car fleet-list) 'get-y) onderkant) 'onderkant)
          (else (fleet-te-ver? (cdr fleet-list))))))
    
    ; Deze hulp-procedure geeft de omgekeerde richting weer van een bepaalde richting.
    
    (define (omgekeerde richting)
      (cond ((eq? richting 'right) 'left)
            ((eq? richting 'left) 'right)))
    
    ; Procedure om een fleet-list te maken die gegeven zal worden aan fleet-ADT.
    
    (define (health species)
      (define l (list '(yellow 1) '(purple 2) '(green 3)))
      (cadr (assoc species l))) 
    
    ; Fleet wordt gemaakt met de buienste monstertjes van de fleet eerst.
    
    (define (maak-fleet-list spec-1 spec-2 spec-3)
      (define (iter counter result)
        (define correctie-links (+ 0.125 (* counter 0.05)))
        (define correctie-rechts (- 0.875 (* counter 0.05)))
        (if (=  counter 8)
            result
            (iter (+ counter 1) (append  result
                                         (list (maak-monster-adt spec-3 correctie-links  0.1   (health spec-3))
                                               (maak-monster-adt spec-2 correctie-links  0.2   (health spec-2))
                                               (maak-monster-adt spec-1 correctie-links  0.3   (health spec-1))
                                               (maak-monster-adt spec-3 correctie-rechts 0.1   (health spec-3))
                                               (maak-monster-adt spec-2 correctie-rechts 0.2   (health spec-2))
                                               (maak-monster-adt spec-1 correctie-rechts 0.3   (health spec-1)))
                                         ))))
      (iter 0 '()))
    
    
    ;////////////////;
    ;+++ POWER UP +++;
    ;////////////////;
    
    ; Maak een power-up-aan en display deze dan.
    
    (define (create-power-up species x)
      (let* ((y        0.8)
             (power-up (maak-power-up-adt species x y speltijd)))
        ((power-up 'teken!) teken-adt)
        (set! power-list (cons power-up power-list))))
    
    ; Deze procedure geeft een random soort power-up terug.
    
    (define (random-power-up)
      (define species (vector 'yellow 'white 'red))
      (define random-ref (modulo (exact-round (* 10 (random (make-pseudo-random-generator)))) 3))
      (vector-ref species random-ref))
    
    ; Procedure die random x-waarde geeft voor een power-up.
    
    (define (random-x)
      (random (make-pseudo-random-generator)))
    
    
    ;----------------------------------------------
    ;---------------- LOOP PROCEDURES -------------
    ;----------------------------------------------
    
    ;///////////////// SPELTIJD-LOOP ////////////////;
    
    ; Verhoog de speltijd met 1 seconde.
    
    (define (speltijd-loop)
      (set! speltijd (+ speltijd 1)))
    
    
    ;///////////////// BULLET-LOOP ////////////////;
    
    (define (bullet-loop bullet-adt)
      (let ((y               (bullet-adt 'get-y)))
        (cond
          ((<= y 0)
           ((bullet-list-adt 'verwijder-bullet!) bullet-adt teken-adt))
          
          (else
           
           ; De bullet zet een stap vooruit.
           
           (bullet-adt 'move!)
           
           ;Er wordt gecontroleerd of de bullet een monster van de fleet heeft geraakt.
           
           (hit-detection bullet-adt fleet-adt)))))
    
    ;///////////////// POWER-UP-LOOP ////////////////;
    
    (define (power-up-loop power-up-adt)
      (let ((species (power-up-adt 'species)))
        
        ; Er wordt gekeken wat er in het spel moet veranderen als er een bepalde power-up wordt geraakt.
        
        (when (hit-power-up power-up-adt rocket-adt)
          (cond
            
            ;Wanneer de gele power-up is geraakt, gaat de fleet voor een tijd niet naar beneden.
            
            ((eq? species 'yellow)
             (set! yellow-power-up-time 5)
             (set! vertical-speed-fleet 0))
            
            ; Wanneer de witte power-up wordt geraakt gaat de fleet zeer snel naar beneden.
            
            ((eq? species 'white)
             (set! white-power-up-time 5)
             (set! fleet-refresh 50))
            
            ; Wordt de rode power-up geraakt, dan sciet de rocket sterkere bullets.
            
            ((eq? species 'red)
             (set! red-power-up-time 5)
             (set! bullet-impact (+ bullet-impact 1))))
          
          ; Verwijder de power-up uit power-lijst en van het scherm.
          
          (set! power-list (remove! power-up-adt power-list))
          ((power-up-adt 'verwijder!) teken-adt))))
    
    
    ;///////////////// FLEET-LOOP /////////////////;
    
    (define (fleet-loop fleet)
      (let ((fleet               (fleet-adt 'fleet))) 
        
        (cond
          
          ; Als een monster van de fleet op het einde is, is het GAME OVER.
          
          ((eq? (fleet-te-ver? fleet) 'onderkant)  ; Dit getal geeft de grens weer tot waar de monsters mogen komen voordat het game over is.
           ((fleet-adt 'voor-alle-monsters) (lambda (monster-adt)((fleet-adt 'verwijder-monster!) monster-adt teken-adt))) ;  VErwijder alle monsters van het scherm.
           ((bullet-list-adt 'voor-alle-bullets) (lambda (bullet-adt) ((bullet-list-adt 'verwijder-bullet!) bullet-adt teken-adt))) ; Verwijder alle bullets van het scherm.
           (set! score (scorebord-adt 'get-score)) ; Geeft de waarde van het scorebord-ADT aan de variabele score.
           (scorebord-adt 'reset!) ; Reset het scorebord.
           (gameover-menu) ; Ga naar het game-over-menu.
           (set! fleet-refresh fleet-refresh-rate) ; Zet de fleet refresh terug naar zijn oorspronkelijke waarde.
           (set! level 1) ; Zet level op 1
           (set! speltijd 0) ; Zet spelyijd op 0
           (for-each (lambda (power-up) ((power-up 'verwijder!) teken-adt)) power-list) ; Alle power-ups worden verwijderd van het scherm.
           ((rocket-adt 'verwijder!) teken-adt)) ; Rocket wordt van scherm verwijderd.
          
          ; Als een monster de rand heeft bereikt en de fleet mag dalen, zet de fleet een stap naar beneden.
          
          ((and down? (fleet-te-ver? fleet))
           (set! fleet-richting (omgekeerde fleet-richting))
           ((fleet-adt 'move!) 'down vertical-speed-fleet)
           (set! down? #f))
          
          ; Als de fleet leeg is, is de level gedaan en komt het level complete scherm op het venster.
          
          ((null? (fleet-adt 'fleet))
           (set! speltijd 0)
           (set! fleet-refresh (- fleet-refresh 50)) ; Fleet refresh wordt verlaagt waardoor de fleet zich sneller zal bewegen.
           (for-each (lambda (power-up) ((power-up 'verwijder!) teken-adt)) power-list) ; Alle power-ups worden verwijderd van het scherm.
           ((bullet-list-adt 'voor-alle-bullets) (lambda (bullet-adt) ((bullet-list-adt 'verwijder-bullet!) bullet-adt teken-adt))) ; Verwijder alle bullets van het scherm.
           (levelcomplete-menu) ; Ga naar het level complete menu.
           ((rocket-adt 'verwijder!) teken-adt) ; Rocket wordt van scherm verwijderd
           (set! level (+ level 1))) ; Verhoog het level met 1.
          
          
          
          ; Als de fleet de rand niet heeft bereikt, dan mag het verder gaan in de aangegeven righting.
          
          (else ((fleet-adt 'move!) fleet-richting horizontal-speed-monster) (set! down? #t)))))
    
    
    
    
    ;----------------------------------------------
    ;----------------- VOORBEREIDING --------------
    ;----------------------------------------------
    
    ;Procedure die de volledige fleet-lijst meegeeft aan het fleet-ADT in het begin. Aan de hand van het level worden verschillende fleet's gemakt.
    
    (define (maak-level level)
      (let ((lambda (lambda (monster-adt) ((fleet-adt 'voeg-monster-toe!) monster-adt teken-adt))))
        
        (cond ((= level 1)
               (for-each lambda (maak-fleet-list 'yellow 'yellow 'yellow)))
              ((= level 2)
               (for-each lambda (maak-fleet-list 'yellow 'yellow 'purple)))
              ((= level 3)
               (for-each lambda (maak-fleet-list 'purple 'purple 'purple)))
              ((= level 4)
               (for-each lambda (maak-fleet-list 'yellow 'purple 'green)))
              ((= level 5)
               (for-each lambda (maak-fleet-list 'purple 'green 'green)))
              ((>= level 6)
               (for-each lambda (maak-fleet-list 'green 'green 'green))))))
    
    (maak-level level)

    ; Teken het scorebord.
    
    ((scorebord-adt 'teken!) teken-adt)
    
    ; Teken de timer.
    
    ((teken-adt 'teken-timer!) 0)
    
    
    ;----------------------------------------------
    ;------------------ TOETSFUNCTIE --------------
    ;----------------------------------------------
    
    ; Deze functie zal meegegeven worden aan het teken-adt en vervolgens graphics.rkt.
    ; De procedure controleert ook invoer van het toetsenbord.
    
    (define (game-toets-functie toets)
      (let ((te-links?                (<= (rocket-adt 'get-x) 0.02))
            (te-rechts?               (>= (rocket-adt 'get-x) 0.92)))
        (cond
          
          ; Rechtertoets ingeduw en niet voorbij de rechterkant.
          
          ((and (eq? toets 'right) (not te-rechts?)) 
           ((rocket-adt 'move!) 'rechts))
          
          ; Linkertoets ingeduwd en niet voorbij de linkerkant.
          
          ((and (eq? toets 'left) (not te-links?)) 
           ((rocket-adt 'move!) 'links))
          
          ; Control is ingedrukt.
          
          ((eq? toets 'control) 
           (create-bullet!))
          
          ; P toets wordt ingedrukt
          
          ((eq? toets '#\p)
           (pauze-menu)))))

    
    ;----------------------------------------------
    ;-------------------- SPELLLUS ----------------
    ;----------------------------------------------
    (define dt 0)  ;NIEUW!
    
    ; Deze wordt meegegeven aan graphics en wordt bij elke tik van het spel uitgevoerd.
    
    (define (spel-lus-functie delta-tijd)
      
      ; Bij elke oproep van de procedure wordt bullet-tijd en fleet-tijd geupdated.
      
      (set! bullet-tijd (+ bullet-tijd delta-tijd))
      (set! fleet-tijd (+ fleet-tijd delta-tijd))
      (set! power-up-tijd (+ power-up-tijd delta-tijd))
      (set! tijd-parameter (+ tijd-parameter delta-tijd))
      (set! dt (+ dt delta-tijd))
      
      (when (> dt 22)
        (set! dt 0)  
        
        
        ;///////////////// BULLET-LOOP ////////////////
        ; Wanneer de bullet-tijd groter is dan bullet-snelhied wordt de bullet-loop in gang gezet.
        ; Deze loop zal controleren of een bullet mag verderbewegen en zal deze dan ook doen bewegen.
        
        (when (> bullet-tijd bullet-refresh-rate)
          
          ; De loop wordt over de bullet-list gemapt.
          
          ((bullet-list-adt 'voor-alle-bullets) (lambda (bullet-adt) (bullet-loop bullet-adt)))
          ; Bullet-tijd wordt opnieuw op nul gezet.
          
          (set! bullet-tijd 0))
        
        
        
        ;///////////////// FLEET-LOOP ////////////////
        ; Analoog met bullet-loop.
        
        (when (> fleet-tijd fleet-refresh)
          
          ; De fleet-loop procedure wordt over alle monsters van de fleet gemapt.
          
          (fleet-loop fleet-adt)
          
          
          ;NODIG: EEN PROCEDURE OM DE FLEET SNELLER TE MAKEN! ;TEST
          
          ; Zet de klok elke seconde vooruit.
          
          ((teken-adt 'teken-timer!) speltijd)
          
          
          (set! fleet-tijd 0))
        
        ;///////////////// SPELTIJD-LOOP ////////////////
        
        (when (> tijd-parameter 1000)
          (speltijd-loop)
          (set! tijd-parameter 0)
          
          
          ; Er wordt elke seconde gekeken of er 10 sec zijn voorbijgegaan. Als het zo is, wordt er een power-up aangemaakt
          
          (when (= (modulo speltijd 5) 0)
            (create-power-up (random-power-up) (random-x)))
          
          (cond ((= yellow-power-up-time 0)
                 (set! vertical-speed-fleet vertical-speed-monster))
                (else (set! yellow-power-up-time (- yellow-power-up-time 1))))
          
          (cond ((= white-power-up-time 0)
                 (set! fleet-refresh fleet-refresh-rate))
                (else (set! white-power-up-time (- white-power-up-time 1))))
          
          (cond ((= red-power-up-time 0)
                 (set! bullet-impact (- bullet-impact 1)))
                (else (set! red-power-up-time (- red-power-up-time 1))))
          
          
          
          ;///////////////// POWER-UP-LOOP ////////////////
          ; Wordt gebruikt voor de collision detetctie tussen een rocket en een power-up
          
          (when (> power-up-tijd power-up-refresh-rate)
            
            ; Voer de loop uit op alle power-ups van power-list.
            
            (for-each (lambda (power-up) (power-up-loop power-up)) power-list)
            
            ; Opkuisprocedure voor power-ups die al langer dan 5 seconden op het scherm staan.
            
            (for-each (lambda (power-up)
                        (when (> (- speltijd (power-up 'creation-time)) 5)
                          (set! power-list (remove! power-up power-list))
                          ((power-up 'verwijder!) teken-adt)))
                      power-list)
            
            (set! power-up-tijd 0)))

        ; Hertekenen van de spelcomponenten bij elke tik.
        
        ((rocket-adt 'teken!) teken-adt)
        ((bullet-list-adt 'teken!) teken-adt)
        ((fleet-adt 'teken!) teken-adt)))
    
    ; Callbacks naar graphics.rkt via het teken-ADT.
    
    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie)
    ((teken-adt 'set-toets-functie!) game-toets-functie))
  
  ;----------------------------------------------
  ;-------------------- DISPATCH ----------------
  ;----------------------------------------------
  
  (define (dispatch-spel msg)
    (cond
      ((eq? msg 'start-menu) (start-menu))          
      (else
       (display "ERROR: ")
       (display msg)
       (display " IS AN ILLEGAL REQUEST TO SPEL-ADT"))))
  
  dispatch-spel)


