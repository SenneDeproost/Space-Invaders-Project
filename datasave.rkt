#lang racket

(require "data.rkt")
(require 2htdp/batch-io)

(provide
 update-highscores!
 clear-highscores!
 entry-highscore
 save-file)


;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Highscores worden bijgehouden als vectoren in vectoren. Een highscore-entry is een vector met op de eerste positie een naam (quote)
; en op de tweede plaats de score (number).

; In dit deel van de file wordt de highscore-vector die in data.rkt zit gewijzigd en gesorteerd.

; Abstracties.

(define (name vector) (vector-ref vector 0)) ; Geef de naam

(define (score vector)(vector-ref vector 1)) ; Geef de score

; Procedure die gebruik wordt om de highscores in vectorvorm aan te passen.

(define (insert-highscore new-name new-score)
  (let ((entry                    (vector new-name new-score))
        (highscores               save-file))
    
    ; Iteratieve procedure om toe te voegen.
    (define (iter counter vector)
      
      (cond
        ; Als 'empty voorkomt in de vector, dan wordt op deze plaats de entry opgeslagen.
        
        ((equal? (vector-ref highscores counter) 'empty)
         (vector-set! highscores counter entry)
         highscores)
        
        ; Als 'end bereikt is aan het eind van de vector, dan is de vector vol en worden de highscores teruggegeven (weliswaar onveranderd).
        
        ((eq? (vector-ref vector counter) 'end)
         highscores)
        
        ; Als er een score voorkomt die kleiner is die van de nieuwe entry, worden alle entries in de vector vanaf deze plaats één plaats opgeschoven naar rechts.
        
        ((> new-score (score (vector-ref vector counter)))
         (move-to-right entry counter vector))
        
        ; Voor alle andere gevallen.
        
        (else (iter (+ counter 1) vector))))
    
    ; Hulpprocedure om de entries van de vector te verplaatsen zodat er plaat vrij komt voor een nieuwe entry.
    ; Prev is de vorige entry van de vector en pos is de positie vanaf waar er entries moeten verplaatst worden.
    
    (define (move-to-right prev pos vector)
      (define hulpje (- (vector-length vector) pos)) ; Wordt om de te weten hoe ver de procedure de storage move moet doen in de vector.
      (define (iter counter prev vector)
        (cond
          
          ; Als het einde van de vector is bereikt, geef dan de vector terug.
          
          ((= hulpje 0)
           highscores)
          
          ; Voor alle andere gevallen: zet de vorige entry in de huidige en ga verder.
          
          (else (let ((buffer (vector-ref vector counter)))
                  (vector-set! vector counter prev)
                  (set! hulpje (- hulpje 1))
                  (iter (+ counter 1) buffer vector)))))
      (iter pos prev vector))
    
    (iter 0 highscores)))

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Dit deel van de code zal instaan voor de translatie van de highscore datastructuur naar een vector in stringvorm.
; Deze string kan nadien weg geschreven worden naar de save file.

; Wat aan het begin van de save file moet staan.

(define file-config "#lang racket \n (provide save-file) \n") ; Zorgt ervoor dat de dile in de juiste taal staat en de juiste elementen provide.

; Zal een vector maken in string vorm.

(define (vector-string exp-string)
  (string-append "(vector" exp-string ")"))

; Maakt een definitie in string vorm.

(define (definition var-string exp-string)
  (string-append "(define " var-string exp-string ")"))

; Maak een entry vector met een gegeven naam in string vorm en een nummer.

(define (entry-string name score)
  (define name-string (symbol->string name)) ; Zet de naam als quote om naar een string.
  (define score-string (number->string score)) ; Zet de score als getal om naar een string.
  (define content (string-append "'" name-string "\n" score-string)) ; Voegt name-string en score-string samen.
  (vector-string content))

; Er wordt gekeken of een bepaalde entry empty is of dat deze een echte entry is.

(define (content-string content)
  (if (eq? content 'empty)
      "'empty" ; Geef de quote empty als het geen exhte entry is.
      (entry-string (name content) (score content)))) ; Maak een entry-string aan.

; Parameters moeten entries zijn van de highscore-vector.

(define (text-string 1st 2nd 3th 4th 5th)
  (vector-string
   (string-append
    (content-string 1st)
    (content-string 2nd)
    (content-string 3th)
    (content-string 4th)
    (content-string 5th)
    "'end"))) ; Op het einde wordt er 'einde geappend om het einde van de vector aan te duiden.

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Dit gedeelde moet instaan voor het updaten van de save file.

; Een naam en een score worden meegegeven als parameters.
; De procedure insert-highscore wordt uitgevoerd en geeft een gesorteerde highscore vector terug.
; Deze vector wordt dan omgezet entry per entry met de procedure text-string.

(define (insert-new-score name score)
  (define highscore-vector (insert-highscore name score))
  (text-string
   (vector-ref highscore-vector 0)
   (vector-ref highscore-vector 1)
   (vector-ref highscore-vector 2)
   (vector-ref highscore-vector 3)
   (vector-ref highscore-vector 4)))

(define (updated-scores name score) (string-append file-config (definition "save-file \n" (insert-new-score name score))))

; Procedure om de highscores te updaten. 

(define (update-highscores! new-name new-score)
  (write-file "data.rkt" (updated-scores new-name new-score)))

(define empty-vector "(vector 'empty 'empty 'empty 'empty 'empty 'end)")

; Procedure om alle entries van de highscore vector te verwijderen.

(define (clear-highscores!)
  (write-file "data.rkt" (string-append file-config (definition "save-file \n" empty-vector))))

(define (show-data) (substring (read-file "data.rkt") 44))

; Geeft een bepaalde entry van de vector terug in vector vorm.

(define (entry-highscore rang)
  (let ((highscores save-file))
    (vector-ref save-file (- rang 1))))
