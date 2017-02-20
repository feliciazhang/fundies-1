;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; string alphabetic...something
;;; Felicia Zhang and Sarah Coffen
;;; Assignment 10 -> Typaholic!

(define TEXT-SIZE 15)
(define GRID-HEIGHT 40)                           
(define GRID-WIDTH 40)
(define CELL-HEIGHT 15)
(define CELL-WIDTH 15)
(define ACTIVE-COLOR "green")
(define TYPING-COLOR "purple")
(define STUCK-COLOR "blue")
(define SCENE-HEIGHT (* GRID-HEIGHT CELL-HEIGHT))
(define SCENE-WIDTH (* GRID-WIDTH CELL-WIDTH))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define BACKGROUND (rectangle SCENE-WIDTH (+ SCENE-HEIGHT 25) 'solid 'white))
(define SCREEN (place-image SCENE (/ SCENE-WIDTH 2) (/ SCENE-HEIGHT 2) BACKGROUND))
;; speed is number of cells moved down per tick
(define SPEED 1)

;; a World is a (make-world LoFW LoSW Typed-word Score)
(define-struct world [lofw losw typed-word score])
(define begin (make-world empty empty empty 0))

;; A Word is one of:
; - Falling-Word
; - Stuck-Word
; - Typed-Word

;; a Falling-Word is a (make-falling-word Posn String)
(define-struct falling-word [location text])
(define shiny (make-falling-word (make-posn 4 16) "shiny"))

;; a Stuck-Word is a (make-stuck-word Posn String)
(define-struct stuck-word [location text])
(define coconut (make-stuck-word (make-posn 20 40) "coconut"))

;; an LoFW is a List of Falling-Words
(define lofw1(list (make-falling-word (make-posn 4 16) "island")
                   (make-falling-word (make-posn 6 5) "boat")))

;; an LoSW is a List of Stuck-Words
(define losw1 (list (make-stuck-word (make-posn 22 39) "chicken")
                    (make-stuck-word (make-posn 18 33) "flower")))

;; a Typed-Letter is a (make-typed-letter Posn String)
(define-struct typed-letter [location letter])
(define sea1 (make-typed-letter (make-posn 270 615) "s"))
(define sea2 (make-typed-letter (make-posn 280 615) "e"))
(define sea3 (make-typed-letter (make-posn 290 615) "a"))

;; a Typed-Word is a List of Typed Letters
(define tw1 (list sea1 sea2 sea3))

;; a Score is a Number
;; calculated as the inverse frequency times number of ticks elapsed

;; a Vocabulary is an LoS
;; which is the selection of words that will be used in the game
(define vocabulary (list "sail" "heart" "moana" "wind" "water" "canoe" "explorers"
                         "tattoo" "maui" "away" "island" "waves" "tide" "coconut"))


#;
(define (world-template w)
  ...(lofw-template (world-lofw w))...
  ...(losw-template (world-losw w))...
  ...(typed-word-template (world-typed-word w))...
  ...(score-template (world-score w))...)

#;
(define (lofw-template lofw)
  (cond [(empty? lofw)...]
        [else ...(falling-word-template (first lofw))...
              ...(lofw-template (rest lofw))...]))

#;
(define (losw-template losw)
  (cond [(empty? losw)...]
        [else ...(stuck-word-template (first losw))...
              ...(losw-template (rest losw))...]))

#;
(define (word-template word)
  (cond [(falling-word? word)...]
        [(stuck-word? word)...]
        [(typed-word? word)...]))

#;
(define (falling-word-template fw)
  ...(posn-template (falling-word-location fw))...
  ...(falling-word-text fw)...
  ...(falling-word-speed fw)...)

#;
(define (stuck-word-template sw)
  ...(posn-template (stuck-word-location sw))...
  ...(stuck-word-text sw)...)

#;
(define (typed-word-template tw)
  (cond [(empty? tw) ...]
        [else ...(typed-letter-template (first tw))...
              ...(typed-word-template (rest tw))...]))

#;
(define (typed-letter-template l)
  ...(typed-letter-location l)...
  ...(typed-letter-letter l)...)

#;
(define (vocabulary-template los)
  (cond [(empty? los)...]
        [else ...(first los)...
              ...(typed-word-template (rest los))...]))


;; next-world: World -> World
;; Changes the world state on every tick
#;(define (next-world w)
  (make-world (words-are-falling (still-falling w))
              (append (falling-to-stuck w) (world-losw w)))
              (typed-word-template (world-typed-word w))
              ...(score-template (world-score w))...))

;; new-falling-word: Vocabulary -> String
;; Retrieves a word from the list of vocabulary to fall
(define (new-falling-word vocabulary)
  (cond [(= (random (length vocabulary)) 1) (first vocabulary)]
        [else (new-falling-word (rest vocabulary))])) 


;; maybe-new-word?: World -> Boolean
;; New word generated every other tick



;; words-are-falling: LoFW -> LoFW
;; Falling words move one row down on the grid on every tick
(define (words-are-falling lofw)
  (cond [(empty? lofw) empty]
        [else (cons (move-down (first lofw))
              (words-are-falling (rest lofw)))]))

(check-expect (words-are-falling lofw1)
              (list (make-falling-word (make-posn 4 17) "island")
                    (make-falling-word (make-posn 6 6) "boat")))

;; still-falling: World -> LoFW
;; Keeps a falling word a falling word if it is not touching the bottom or another word
(define (still-falling w)
  (if (not(or (touch-bottom? (first (world-lofw w)))
          (touching-stuck-word? (first (world-lofw w)))))
      (cons (first (world-lofw w))
            (still-falling (rest (world-lofw w))))
      (still-falling (rest (world-lofw w)))))

;(check-expect (falling-to-stuck 

;; falling-to-stuck: World -> LoSW
;; Turns a falling word to a stuck word when it touches the bottom or another word
(define (falling-to-stuck w)
  (if (or (touch-bottom? (first (world-lofw w)))
          (touching-stuck-word? (first (world-lofw w))
                                (world-losw w)))
      (cons (first (world-lofw w))
            (falling-to-stuck (rest (world-lofw w))))
      (falling-to-stuck (rest (world-lofw w)))))

;; touch-bottom?: Falling-Word -> Boolean
;; Is a falling word touching the bottom of the screen?
(define (touch-bottom? fw)
  (>= (posn-y (falling-word-location (move-down fw))) GRID-HEIGHT))

(check-expect (touch-bottom? (make-falling-word (make-posn 4 40) "water")) #true)
(check-expect (touch-bottom? (make-falling-word (make-posn 4 4) "dreaming")) #false)

;; move-down: Falling-Word -> Falling-Word
;; Returns the falling word as down 1 cell
(define (move-down fw)
  (make-falling-word (make-posn (posn-x (falling-word-location fw))
                                (+ SPEED (posn-y (falling-word-location fw))))
                     (falling-word-text fw)))

(check-expect (move-down (make-falling-word (make-posn 4 40) "water"))
              (make-falling-word (make-posn 4 41) "water"))
        
#|
;; posn-to-grid: Posn -> Posn
;; converts the Posn value to a Grid value
(define (posn-to-grid p)
  (make-posn (/ (posn-x p) CELL-WIDTH)
             (/ (posn-y p) CELL-HEIGHT)))

(check-expect (posn-to-grid (make-posn 45 15)) (make-posn 3 1))
|#

;; touching-stuck-word?: Falling-Word LoSW -> Boolean
;; Is a falling word touching a stuck word?
(define (touching-stuck-word? fw losw)
  (cond [(empty? (first (losw))) #true]
        [(not (and (in-word-width? (text-to-image (first losw)) fw))
                       (in-word-height? (text-to-image (first losw)) fw))
             (touching-stuck-word? fw (rest losw))]
        [else #false]))

;; text-to-image: String -> Image
;; converts a string of text into an image of text
(define (text-to-image s)
  (text s TEXT-SIZE 'blue))

;; in-word-width?: Image Posn -> Boolean
;; returns true if the posn is within the image's width range
(define (in-word-width? i p)
  (and (<= (+ (posn-x p) (/ (image-width i) 2)))
       (>= (- (posn-x p) (/ (image-width i) 2)))))

;; in-word-height?: Image Posn -> Boolean
;; returns true if the posn is within the image's height range
(define (in-word-height? i p)
  (and (<= (+ (posn-y p) (/ (image-height i) 2)))
       (>= (- (posn-y p) (/ (image-height i) 2)))))

;; render: World -> Image
;; Renders the words in the world state on the board
;; and shows the final score



;; draw-falling-words: LoFW -> Image
;; Renders the words that are falling
(define (draw-falling-words lofw)
  (cond [(empty? lofw) SCREEN]
        [else (place-image/grid (text (falling-word-text (first lofw)) TEXT-SIZE ACTIVE-COLOR)
                                (posn-x (falling-word-location (first lofw)))
                                (posn-y (falling-word-location (first lofw)))
                                (draw-falling-words (rest lofw)))]))

(check-expect (draw-falling-words lofw1) (place-image (text "island" 15 'green) 60 232.5
                                                      (place-image (text "boat" 15 'green) 90 67.5
                                                                   SCREEN)))  

;; draw-stuck-words: World -> Image
;; Renders words stuck on screen
(define (draw-stuck-words losw)
  (cond [(empty? losw) SCENE]
        [else (place-image/grid (text (stuck-word-text (first losw)) TEXT-SIZE STUCK-COLOR)
                                (posn-x (stuck-word-location (first losw)))
                                (posn-y (stuck-word-location (first losw)))
                                (draw-stuck-words (rest losw)))]))

(check-expect (draw-stuck-words losw1) (place-image (text "chicken" 15 'blue) 330 577.5
                                                    (place-image (text "flower" 15 'blue) 270 487.5
                                                                 SCENE)))

;; draw-player-typing: Typed-Word -> Image
;; Renders the letters that the player is typing
(define (draw-player-typing tw)
  (cond [(empty? tw) SCREEN]
        [else (place-image (text (typed-letter-letter (first tw)) TEXT-SIZE TYPING-COLOR)
                           (posn-x (typed-letter-location (first tw)))
                           (posn-y (typed-letter-location (first tw)))
                           (draw-player-typing (rest tw)))]))

(check-expect (draw-player-typing tw1)
              (place-image (text "s" 15 'purple) 270 615
                           (place-image (text "e" 15 'purple) 280 615
                                        (place-image (text "a" 15 'purple) 290 615
                                                     SCREEN))))

;; draw-score: World -> Image
;; Renders the final score on screen
(define (draw-score w)
  (overlay (text (string-append "Your score is " (number->string (world-score w))) 60 'blue)
           SCREEN))

;(check-expect )

;; place-image/grid: Image Number Number Image -> Image
;; Places image between grid values
(define (place-image/grid img1 n1 n2 BG)
  (place-image img1
               (* CELL-WIDTH n1)
               (- (* CELL-HEIGHT n2) (/ CELL-HEIGHT 2))
               BG))

(check-expect (place-image/grid (text "horizon" 12 'blue) 4 4 SCREEN)
              (place-image (text "horizon" 12 'blue) 60 52.5 SCREEN))


;; type: World KeyEvent -> World
;; User inputs letters with the alphabet keyboard to type a word
;; and submits them to remove matching falling words when Enter is pressed
;; and backspace can be used to delete letters typed

;; submit: World -> World
;; Removes falling words from the screen that match the word that the player types

;; word-match?: Typed-Word LoFW -> Boolean
;; Does the typed word match one of the falling words?
(define (word-match? tw lofw)
  (cond [(empty? tw) #false]
        [(empty? lofw) #false]
        [(string=? (letters-to-word tw) (falling-word-text (first lofw))) #true]
        [else (word-match? tw (rest lofw))]))

(check-expect (word-match? tw1 empty) #false)
(check-expect (word-match? empty lofw1) #false)
(check-expect (word-match? tw1 lofw1) #false)
(check-expect (word-match? tw1 (list (make-falling-word (make-posn 4 16) "sea"))) #true)

;; letters-to-word: Typed-Word -> String
;; Converts list of strings that user types into a single string
(define (letters-to-word tw)
  (cond [(empty? tw) ""]
        [else (string-append (typed-letter-letter(first tw))
                             (letters-to-word (rest tw)))]))

(check-expect (letters-to-word tw1) "sea")

;; delete: Typed-Word -> Typed-Word
;; Removes the last letter that the player typed
(define (delete tw)
  (cond [(empty? tw) empty]
        [else (reverse (rest (reverse tw)))]))

(check-expect (delete tw1) (list sea1 sea2))
(check-expect (delete empty) empty)

;; add-typing: Typed-Word String -> Typed-Word
;; Adds the letter player types
(define (add-typing tw letter)
  (reverse (cons (make-typed-letter (make-posn (+ 270 (* 10 (length tw))) 615) letter)
                 (reverse tw))))

(check-expect (add-typing empty "s") (list sea1))
(check-expect (add-typing tw1 "s")
              (list sea1 sea2 sea3 (make-typed-letter (make-posn 300 615) "s")))

;; end-game: World -> Boolean
;; Ends the game when 

;; scoring: World -> World
;; Calculates player's score also wtf



;; main: Number -> Number
#;
(define (main tick-speed)
  (big-bang empty
   (on-tick next-world tick-speed)
   (to-draw render)
   (on-key type)
   (stop-when end-game render)))