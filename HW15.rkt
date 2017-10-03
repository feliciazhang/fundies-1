;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;; Felicia Zhang and Benjamin Herzberg
;;; Assignment 15 -> Typaholic! Again!

(define TEXT-SIZE 15)
; ACTIVE-COLOR is the color of the falling words
(define ACTIVE-COLOR 'green)
; TYPING-COLOR is the color of the word that the player inputs
(define TYPING-COLOR 'purple)
; STUCK-COLOR is the color of the words that are stuck at the bottom
(define STUCK-COLOR 'blue)
; The following grid and cell definitions split the game area into gridded coordinates
(define GRID-HEIGHT 40)                           
(define GRID-WIDTH 40)
(define CELL-HEIGHT 15)
(define CELL-WIDTH 15)
(define SCENE-HEIGHT (* GRID-HEIGHT CELL-HEIGHT))
(define SCENE-WIDTH (* GRID-WIDTH CELL-WIDTH))
; SCENE is the screen on which the falling and stuck words appear
(define SCENE (rectangle SCENE-WIDTH SCENE-HEIGHT 'outline 'black))
; BACKGROUND includes the region where the player's typed word appears
(define BACKGROUND (rectangle SCENE-WIDTH (+ SCENE-HEIGHT 25) 'outline 'black))
; SCREEN is the game play area
(define SCREEN (place-image SCENE (/ SCENE-WIDTH 2) (/ SCENE-HEIGHT 2) BACKGROUND))
; SPEED is number of cells moved down per tick
(define SPEED 1)

; vocabulary is the list of words used in the game
(define vocabulary (list "sail" "heart" "moana" "wind" "water" "canoe" "explorers"
                         "tattoo" "maui" "away" "island" "waves" "tide" "coconut"))


; a World is a (make-world LoFW LoSW Word Number Number)
(define-struct world [lofw losw typed-word ticks frequency])
(define world1 (make-world empty empty empty 0 0.3))
; LoFW = List of Falling Words
; LoSW = List of Stuck Words
; ticks represents the amount of time elapsed in the game
; frequency is user inputted as how often each tick occurs

; A LoW is a
; empty
; (cons Word LOW)
#;
(define (low-template low)
  (cond [(empty? low)...]
        [else ...(first low)...(low-template (rest low))...]))

; A LoFW is a LoW
; A LoSW is a LoW

; A Word is a LOL (List of Letters)
#;
(define (word-template lol)
  (cond [(empty? lol)...]
        [else ... (first lol)
              ...(word-template (rest lol))]))

; a Letter is a (make-letter String Posn)
(define-struct letter [txt location])

(define sea1 (make-letter "s" (make-posn 17 22)))
(define sea2 (make-letter "e" (make-posn 18 22)))
(define sea3 (make-letter "a" (make-posn 19 22)))

(define sea21 (make-letter "s" (make-posn 17 23)))
(define sea22 (make-letter "e" (make-posn 18 23)))
(define sea23 (make-letter "a" (make-posn 19 23)))

(define pig1 (make-letter "p" (make-posn 20 30)))
(define pig2 (make-letter "i" (make-posn 21 30)))
(define pig3 (make-letter "g" (make-posn 22 30)))

(define cow1 (make-letter "c" (make-posn 20 GRID-HEIGHT)))
(define cow2 (make-letter "o" (make-posn 21 GRID-HEIGHT)))
(define cow3 (make-letter "w" (make-posn 22 GRID-HEIGHT)))

(define sea (list sea1 sea2 sea3))
(define seaB (list sea21 sea22 sea23))
(define pig (list pig1 pig2 pig3))
(define cow (list cow1 cow2 cow3))

(define low1 (list sea pig))

(define world2 (make-world (list sea) (list pig) (list (make-letter "m" (make-posn 1 41))) 2 0.3))
(define world3 (make-world (list sea) (list pig) (list (make-letter "m" (make-posn 1 41))) 1 0.3))
(define world4 (make-world (list cow) empty empty 2 .3))
(define world5 (make-world empty (list cow) empty 2 .3))

; next-world: World-> World
; updates the world on every tick
(define (next-world w)
  (generate-new-moving-word (falling->stuck (move-lofw w))))

(check-expect (world? (next-world world2)) #true)

; move-lofw: World -> World
; moves all falling words and increases ticks by one
(define (move-lofw w)
  (make-world (map move-word (world-lofw w))
              (world-losw w)
              (world-typed-word w)
              (+ 1 (world-ticks w))
              (world-frequency w)))

(check-expect (move-lofw (make-world (list sea) (list cow) empty 2 .3))
              (make-world (list seaB) (list cow) empty 3 .3))

; move-word: Word -> Word
; moves word down
(define (move-word wrd)
  (map move-letter wrd))

(check-expect (move-word sea) seaB)

; move-letter: Letter -> Letter
; moves letter down one row on each tick
(define (move-letter l)
  (make-letter (letter-txt l)
               (make-posn (posn-x (letter-location l))
                          (+ SPEED (posn-y (letter-location l))))))

(check-expect (move-letter sea1) (make-letter "s" (make-posn 17 23)))

; falling->stuck: World -> World
; Adds a falling word to the LoSW when it touches the bottom or another stuck word
; and removes them from falling words
(define (falling->stuck w)
  (make-world (filter (lambda (wrd) (not (on-bottom/word? wrd w))) (world-lofw w))
              (append (world-losw w)
                      (filter (lambda (wrd) (on-bottom/word? wrd w)) (world-lofw w)))
              (world-typed-word w)
              (world-ticks w)
              (world-frequency w)))
              
(check-expect (falling->stuck world4 ) world5)
(check-expect (falling->stuck world5) world5)

; on-bottom/word?: Word World -> World
; checks if word is on another word or on bottom
(define (on-bottom/word? wrd w)
  (if (= GRID-HEIGHT (posn-y (letter-location (first wrd)))) #true
      (ormap (lambda (letr) (touching-words? letr (world-losw w))) wrd)))

(check-expect (on-bottom/word? (list (make-letter "x" (make-posn 3 GRID-HEIGHT))) world2) #true)
(check-expect (on-bottom/word? (list (make-letter "x" (make-posn 20 29))) world2) #true)
(check-expect (on-bottom/word? (list (make-letter "x" (make-posn 3 3))) world1) #false)

; touching-words?: Letter LOW -> Boolean
; Is a letter touching any word in a LoW?
(define (touching-words? letr low)
  (ormap (lambda (wrd) (letters-touching? letr wrd)) low))

(check-expect (touching-words? (make-letter "s" (make-posn 12 13))
                              (list(list (make-letter "s" (make-posn 12 14)))))
              #true)
(check-expect (touching-words? (make-letter "s" (make-posn 12 12))
                              (list (list (make-letter "s" (make-posn 12 14)))))
              #false)

; letters-touching?: Letter Word -> Boolean
; Is a letter touching any of the letters in a word?
(define (letters-touching? ltr wrd)
  (ormap (lambda (l) (posn=? (letter-location l)
                             (make-posn (posn-x (letter-location ltr))
                                        (+ 1 (posn-y (letter-location ltr)))))) wrd))

(check-expect (letters-touching? sea1 seaB) #true)
(check-expect (letters-touching? sea1 pig) #false)

; posn=?: Posn Posn -> Boolean
; are x and y of the posn equivalent?
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(check-expect (posn=? (make-posn 1 2) (make-posn 1 2)) #true)
(check-expect (posn=? (make-posn 1 2) (make-posn 1 3)) #false)

; generate-new-moving-word: World -> World
; adds new falling word to LoFW
(define (generate-new-moving-word w)
  (if (even? (world-ticks w))
      (make-world (add-new-word (world-lofw w))
                                          (world-losw w)
                                          (world-typed-word w)
                                          (world-ticks w)
                                          (world-frequency w)) w))

(check-expect (length (world-lofw (generate-new-moving-word world1))) 1)
(check-expect (length (world-lofw (generate-new-moving-word world2))) 2)
(check-expect (length (world-lofw (generate-new-moving-word world3))) 1)

; add-new-word: LOW -> LOW
; adds new randommly generated word to the list words
(define (add-new-word low)
  (append low (list (get-word-from-vocab (random (length vocabulary))))))

(check-expect (length (add-new-word low1)) (+ 1 (length low1)))
(check-expect (length (add-new-word empty)) 1)

; get-word-from-vocab: Number -> Word
; selects string from vocabulary and turns it into a word
(define (get-word-from-vocab num)
  (los->word (explode (list-ref vocabulary num))))

(check-expect (andmap letter? (get-word-from-vocab 2)) #true)

; los->word: LoS -> Word
; creates new word at top of gamespace
(define (los->word los)
  (local [(define init-x-posn (+ 1 (random(- GRID-WIDTH (length los)))))]
  (add-posn los (make-posn init-x-posn 0))))

(check-expect (andmap letter? (los->word sea)) #true)

; add-posn: LoS Posn -> [Listof Letters]
; adds positions to the LoS to turn them into an LoL
(define (add-posn los pos)
  (cond [(empty? los) empty]
        [else (cons (make-letter (first los) pos)
                    (add-posn (rest los) (make-posn (+ (posn-x pos) 1) 0)))]))

(check-expect (map (lambda (x) (posn-x (letter-location x)))
                   (add-posn sea (make-posn 2 0)))
              (list 2 3 4))


; draw-world: World -> Image
; Renders falling, stuck and typed words on the game screen
(define (draw-world w)
  (local [(define draw (lambda (low color base)
                         (overlay (draw-word (append-words low) color) base)))]
  (overlay (draw-word (world-typed-word w) TYPING-COLOR)
           (draw (world-losw w) STUCK-COLOR
                 (draw (world-lofw w) ACTIVE-COLOR SCREEN)))))

(check-expect (draw-world (make-world (list sea)
                                      (list pig)
                                      (list (make-letter "g" (make-posn 2 41)))
                                      10 0.3))
              (overlay (draw-word (list (make-letter "g" (make-posn 2 41))) 'purple)
                       (overlay (draw-word pig 'blue)
                                (overlay (draw-word sea 'green) SCREEN))))

; append-words: LoW -> LoL
; appends all the words in a LoW to a LoL
(define (append-words low)
      (foldr append empty low))

(check-expect (append-words low1) (list sea1 sea2 sea3 pig1 pig2 pig3))

; draw-word: Word Symbol -> Image
; Renders a word
(define (draw-word lol color)
  (foldr overlay SCREEN (map (lambda (l) (draw-letter l color)) lol)))

(check-expect (draw-word sea 'blue)
              (foldr overlay SCREEN (map (lambda (l) (draw-letter l 'blue)) sea)))

; draw-letter: Letter Symbol -> Image
; Renders a letter
(define (draw-letter l color)
  (place-image/grid (text (letter-txt l) TEXT-SIZE color)
                    (posn-x (letter-location l))
                    (posn-y (letter-location l))
                    BACKGROUND))

(check-expect (draw-letter sea1 'red)
              (place-image/grid (text "s" TEXT-SIZE 'red) 17 22 BACKGROUND))

; place-image/grid: Image Number Number Image -> Image
; Places image between grid values
(define (place-image/grid img posx posy bg)
  (place-image img
               (+ (* CELL-WIDTH posx) (/ CELL-HEIGHT 2))
               (- (* CELL-HEIGHT posy) (/ CELL-HEIGHT 2))
               bg))

(check-expect (place-image/grid (text "h" 12 'blue) 0 4 SCREEN)
              (place-image (text "h" 12 'blue) 7.5 52.5 SCREEN))


; type: World KeyEvent -> World
; User inputs letters with the alphabet keyboard to type a word
; and submits them to remove matching falling words when Enter is pressed
; and Backspace deletes the last letter typed
(define (type w event)
  (cond [(and (string-alphabetic? event) (= (string-length event) 1))
         (make-world (world-lofw w)
                     (world-losw w)
                     (add-typing (world-typed-word w) event)
                     (world-ticks w)
                     (world-frequency w))]
        [(string=? event "\b")
         (make-world (world-lofw w)
                     (world-losw w)
                     (backspace (world-typed-word w))
                     (world-ticks w)
                     (world-frequency w))]
        [(string=? event "\r") (submit w)]
        [else w]))

(check-expect (type world2 "o") (make-world (list sea)
                                            (list pig)
                                            (list (make-letter "m" (make-posn 1 41))
                                                  (make-letter "o" (make-posn 2 41)))
                                            2 0.3))
(check-expect (type world2 "\b") (make-world (list sea) (list pig) empty 2 0.3))
(check-expect (type world2 "\r") (make-world (list sea) (list pig) empty 2 0.3))
(check-expect (type world2 "\f") world2)

; add-typing: Word String -> Word
; Adds the letter that the player types to the typed word
(define (add-typing wrd s)
  (reverse (cons (make-letter s (make-posn (+ 1 (length wrd)) 41))
                 (reverse wrd))))

(check-expect (add-typing  (list (make-letter "m" (make-posn 1 41))) "o")
              (list (make-letter "m" (make-posn 1 41))
                    (make-letter "o" (make-posn 2 41))))
(check-expect (add-typing  empty "o")
              (list (make-letter "o" (make-posn 1 41))))

; backspace: Word -> Word
; Removes last letter from the typed word
(define (backspace wrd)
  (if (empty? wrd) empty
      (reverse(rest (reverse wrd)))))

(check-expect (backspace sea)   (list sea1 sea2))
(check-expect (backspace empty) empty)

; submit: World -> World
; Removes falling words from the LoFW that match the word that the player types
(define (submit w)
  (make-world (filter (lambda (wrd) (not (string=? (implode (map letter-txt wrd))
                                                   (implode (map letter-txt (world-typed-word w))))))
                      (world-lofw w))
              (world-losw w)
              empty
              (world-ticks w)
              (world-frequency w)))

(check-expect (submit (make-world empty empty empty 14 3))
              (make-world empty empty empty 14 3))
(check-expect (submit world2) (make-world (list sea) (list pig) empty 2 0.3))
(check-expect (submit (make-world low1 (list pig) sea 3 0.3))
              (make-world (list pig) (list pig) empty 3 0.3))


; end-game: World -> Boolean
; Ends the game when a stuck word reaches the top of the screen
(define (end-game w)
  (ormap stuck-at-top? (world-losw w)))

(check-expect (end-game (make-world empty
                                    (list (list (make-letter "m" (make-posn 33 1))))
                                    empty
                                    10
                                    0.3)) #true)
(check-expect (end-game (make-world empty (list pig) empty 3 0.3)) #false)

; stuck-at-top?: Word -> Boolean
; Is there a stuck word at the top? (yes true) (no false)
(define (stuck-at-top? wrd)
  (= (posn-y (letter-location (first wrd))) 1))

(check-expect (stuck-at-top? (list (make-letter "a" (make-posn 33 1)))) #true)
(check-expect (stuck-at-top? sea) #false)


; draw-score: World -> Image
; Renders the final score on screen
(define (draw-score w)
  (overlay (text (string-append "Your score is " (number->string (score w))) 60 'blue)
           SCREEN))

(check-expect (draw-score (make-world empty empty empty 10 0.1))
              (overlay (text "Your score is 100" 60 'blue) SCREEN))

; score: World -> Number
; Final score calculated as ticks/frequency
(define (score w)
  (/ (world-ticks w) (world-frequency w)))

(check-expect (score (make-world empty empty empty 10 0.1)) 100)


; main: World -> World
; Plays Typaholic!
(define (main frequency)
  (score (big-bang (make-world empty empty empty 0 frequency)
   (on-tick next-world frequency)
   (to-draw draw-world)
   (on-key type)
   (stop-when end-game draw-score))))