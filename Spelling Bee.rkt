;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Spelling Bee|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

(define DICTIONARY (read-lines "words.txt")) ; (sample file with dictionary words)
(define LITTLE-DICTIONARY (list "explain" "plain" "nail" "lap"))

(define BACKGROUND (rectangle 400 300 "solid" "light blue"))
(define SEPARATOR (rectangle 5 300 "solid" "white"))
(define WORD-SIZE 40)
(define LOG-BACKGROUND (rectangle 200 230 "solid" "light blue"))
(define LOG-WORD-SIZE 20)
(define HEADER-BACKGROUND (rectangle 200 70 "solid" "light blue"))
(define WORD-COLOR "black")
(define MAIN-WORD-COLOR "dark purple")
(define TYPING-WORD-COLOR "white")


(define (play w)
  (big-bang
      w
    (to-draw world->image)
    (on-key key-pressed)))

; world->image : World -> Image
; draws the world
(check-expect (world->image WORLD-1)
              (overlay/align "left" "bottom"
                             (text (number->string (world-score WORLD-1)) WORD-SIZE WORD-COLOR)
                             (beside
                              (beside
                               (overlay
                                (above
                                 (above
                                  (text (world-partial-word WORLD-1) WORD-SIZE TYPING-WORD-COLOR)
                                  (text " " 20 "white"))
                                 (letters->image (world-letters WORLD-1)))
                                BACKGROUND)
                               SEPARATOR)
                              (above
                               (overlay
                                (text "Words so far:" 25 WORD-COLOR)
                                HEADER-BACKGROUND)
                               (overlay
                                (foldr (λ (word-image-1 word-image-2)
                                         (above word-image-2 word-image-1))
                                       empty-image (map (λ (word)
                                                          (text word LOG-WORD-SIZE WORD-COLOR))
                                                        (world-word-log WORLD-1)))
                                LOG-BACKGROUND)))))
(check-expect (world->image (make-world LETTERS-1 "test" (list "tests") 0))
              (overlay/align "left" "bottom" (text (number->string 0) WORD-SIZE WORD-COLOR)
                             (beside
                              (beside
                               (overlay
                                (above
                                 (above
                                  (text "test" WORD-SIZE TYPING-WORD-COLOR)
                                  (text " " 20 "white"))
                                 (letters->image LETTERS-1))
                                BACKGROUND)
                               SEPARATOR)
                              (above
                               (overlay
                                (text "Words so far:" 25 WORD-COLOR)
                                HEADER-BACKGROUND)
                               (overlay
                                (foldr (λ (word-image-1 word-image-2)
                                         (above word-image-2 word-image-1))
                                       empty-image (map (λ (word)
                                                          (text word LOG-WORD-SIZE WORD-COLOR))
                                                        (list "tests")))
                                LOG-BACKGROUND)))))  
(define (world->image w)
  (overlay/align "left" "bottom" (text (number->string (world-score w)) WORD-SIZE WORD-COLOR)
                 (beside
                  (beside
                   (overlay
                    (above
                     (above
                      (text (world-partial-word w) WORD-SIZE TYPING-WORD-COLOR)
                      (text " " 20 "white"))
                     (letters->image (world-letters w)))
                    BACKGROUND)
                   SEPARATOR)
                  (above
                   (overlay
                    (text "Words so far:" 25 WORD-COLOR)
                    HEADER-BACKGROUND)
                   (overlay
                    (word-log-image w)
                    LOG-BACKGROUND)))))

; word-log-image : World -> Image
; makes an image of all the entered words placed above each other
(check-expect (word-log-image
               (make-world (world-letters WORLD-1) (world-partial-word WORLD-1)
                           (list "word1") 1))
              (text "word1" LOG-WORD-SIZE WORD-COLOR))
(check-expect (word-log-image
               (make-world (world-letters WORLD-1) (world-partial-word WORLD-1)
                           (list "word1" "word2") 2))
              (above (text "word2" LOG-WORD-SIZE WORD-COLOR) (text "word1" LOG-WORD-SIZE WORD-COLOR)))
(define (word-log-image w)
  (foldr (λ (word-image-1 word-image-2) (above word-image-2 word-image-1))
         empty-image (map (λ (word)
                            (text word LOG-WORD-SIZE WORD-COLOR))
                          (world-word-log w))))






; a Letters is a [NE-List-of 1String]
; Interpretation: the letters that can be used to make words
(define LETTERS-1 (list "a" "r" "s" "e" "t" "i" "h"))
(define LETTERS-2 (list "h" "i" "j" "k" "l" "m" "n"))
(define LETTERS-3 (list "o" "p" "q" "r" "s" "t" "u"))
(define (letters-temp l)
  (...
   (cond
     [(empty? (rest l)) ... (first l)]
     [(cons? (rest l))
      (... (first l) ...
           ... (letters-temp (rest l)))])))

(define-struct world [letters partial-word word-log score])
; a World is a (make-world [NE-List-of 1String] String [List-of String] NatNum)®
; Interpretation: the set of 7 possible letters to choose from, the current state of the word
; being constructed, the list of words entered, and the current score
(define WORLD-1 (make-world LETTERS-1 "" empty 0))
(define WORLD-2 (make-world LETTERS-2 "hi" empty 0))
(define WORLD-3 (make-world LETTERS-3 "qrs" empty 0))
(define (world-temp w)
  (...
   (... (world-letters)) ...
   (... (world-partial-word w)) ...
   (... (world-word-log w)) ...
   (... (world-score w)) ...))

; letters->image : [NE-List-of 1String] -> Image
; displays letters as an image
(check-expect (letters->image '()) empty-image)
(check-expect (letters->image LETTERS-1)
              (beside (text "a" WORD-SIZE WORD-COLOR)
                      (text "r" WORD-SIZE WORD-COLOR)
                      (text "s" WORD-SIZE WORD-COLOR)
                      (text "e" WORD-SIZE MAIN-WORD-COLOR)
                      (text "t" WORD-SIZE WORD-COLOR)
                      (text "i" WORD-SIZE WORD-COLOR)
                      (text "h" WORD-SIZE WORD-COLOR)))
(check-expect (letters->image LETTERS-1)
              (beside
               (text (first LETTERS-1) WORD-SIZE WORD-COLOR)
               (text (second LETTERS-1) WORD-SIZE WORD-COLOR)
               (text (third LETTERS-1) WORD-SIZE WORD-COLOR)
               (text (fourth LETTERS-1) WORD-SIZE MAIN-WORD-COLOR)
               (text (fifth LETTERS-1) WORD-SIZE WORD-COLOR)
               (text (sixth LETTERS-1) WORD-SIZE WORD-COLOR)
               (text (seventh LETTERS-1) WORD-SIZE WORD-COLOR)))
(check-expect (letters->image (list "d" "g" "t" "u" "a" "e"))
              (beside
               (text "d" WORD-SIZE WORD-COLOR)
               (text "g" WORD-SIZE WORD-COLOR)
               (text "t" WORD-SIZE MAIN-WORD-COLOR)
               (text "u" WORD-SIZE WORD-COLOR)
               (text "a" WORD-SIZE WORD-COLOR)
               (text "e" WORD-SIZE WORD-COLOR)))
(check-expect (letters->image (list "t" "g" "a" "b" "e"))
              (beside
               (text "t" WORD-SIZE WORD-COLOR)
               (text "g" WORD-SIZE MAIN-WORD-COLOR)
               (text "a" WORD-SIZE WORD-COLOR)
               (text "b" WORD-SIZE WORD-COLOR)
               (text "e" WORD-SIZE WORD-COLOR)))
(define (letters->image los)
  (display-image-list (main-letter los)))

; MAKE THE MIDDLE CHARACTER A DIFFERENT COLOR
; (defined as the fourth from last letter, assuming we provide a world with 7 letters)

; main-letter : [NEList-of 1String] -> [List-of Image]
; calls draw-letters to draw the whole list of Letters one at a time and compiles them into a single
; list of Image.
(check-expect (main-letter LETTERS-1)
              (list (text (first LETTERS-1) WORD-SIZE WORD-COLOR)
                    (text (second LETTERS-1) WORD-SIZE WORD-COLOR)
                    (text (third LETTERS-1) WORD-SIZE WORD-COLOR)
                    (text (fourth LETTERS-1) WORD-SIZE MAIN-WORD-COLOR)
                    (text (fifth LETTERS-1) WORD-SIZE WORD-COLOR)
                    (text (sixth LETTERS-1) WORD-SIZE WORD-COLOR)
                    (text (seventh LETTERS-1) WORD-SIZE WORD-COLOR)))
(check-expect (main-letter '())  '())
(define (main-letter los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (cons (draw-letters los)
           (main-letter (rest los)))]))

; draw-letters : [NEList-of 1String] -> Image
; returns an image of each individual Letter in the given list of Letters
; makes the main letter a different color
(check-expect (draw-letters LETTERS-2)
              (text (first LETTERS-2) WORD-SIZE WORD-COLOR))
(check-expect (draw-letters (list "f" "e" "o" "d"))
              (text (first (list "f" "e" "o" "d")) WORD-SIZE MAIN-WORD-COLOR))
(check-expect (draw-letters (list "d"))
              (text (first (list "d")) WORD-SIZE WORD-COLOR))
(define (draw-letters los)
  (if (= 4 (length los))
      (text (first los) WORD-SIZE MAIN-WORD-COLOR)
      (text (first los) WORD-SIZE WORD-COLOR)))

; display-image-list : [List-of Image] -> Image
; takes in a list of Image and produces a single image of all the Letters
(check-expect
 (display-image-list (main-letter LETTERS-1))
 (beside (text (first LETTERS-1) WORD-SIZE WORD-COLOR)
         (beside (text (second LETTERS-1) WORD-SIZE WORD-COLOR)
                 (beside (text (third LETTERS-1) WORD-SIZE WORD-COLOR)
                         (beside (text (fourth LETTERS-1) WORD-SIZE MAIN-WORD-COLOR)
                                 (beside (text (fifth LETTERS-1) WORD-SIZE WORD-COLOR)
                                         (beside (text (sixth LETTERS-1) WORD-SIZE WORD-COLOR)
                                                 (text
                                                  (seventh LETTERS-1) WORD-SIZE WORD-COLOR))))))))
(check-expect (display-image-list '()) empty-image)
(define (display-image-list loi)
  (foldr beside empty-image loi))

; key-pressed : World KeyEvent -> World
; processes a key pressed by the user and updates World with then new partial word
(check-expect (key-pressed WORLD-2 "j") (make-world
                                         (world-letters WORLD-2)
                                         "hij"
                                         (world-word-log WORLD-2)
                                         (world-score WORLD-2)))
(check-expect (key-pressed WORLD-2 "\b") (make-world
                                          (world-letters WORLD-2)
                                          "h"
                                          (world-word-log WORLD-2)
                                          (world-score WORLD-2)))
(check-expect (key-pressed WORLD-2 "\r") (make-world
                                          (world-letters WORLD-2)
                                          "hi"
                                          (world-word-log WORLD-2)
                                          (world-score WORLD-2)))
(check-expect (key-pressed (make-world LETTERS-1 "test" '() 0) "\r") (make-world
                                                                      (world-letters WORLD-1)
                                                                      ""
                                                                      (append (world-word-log WORLD-1)
                                                                              (list "test"))
                                                                      1))
(define (key-pressed w key)
  (cond
    [(can-enter? w key)
     (make-world
      (world-letters w)
      ""
      (cons (world-partial-word w) (world-word-log w))
      ; if all that is true, add the word to the word log
      (change-score w))]
    [(can-delete? w key)
     (make-world
      (world-letters w)
      (substring (world-partial-word w) 0 (- (string-length (world-partial-word w)) 1))
      ; remove the last letter of the string
      (world-word-log w)
      (world-score w))]
    [(can-type? w key)
     (make-world
      (world-letters w)
      (string-append (world-partial-word w) key)
      ; add that key to the end of the partial word
      (world-word-log w)
      (world-score w))]
    [else w]))

; can-enter? : World KeyEvent -> Boolean
; checks if the current partial word meets all the conditions to be
; entered into the word log (returns true if conditions are met)
(check-expect (can-enter? WORLD-2 "\r") #false)
(check-expect (can-enter? (make-world LETTERS-1 "test" (list "tests") 0) "\r") #true)
(define (can-enter? w key)
  (and
   (key=? "\r" key) ; if you enter
   (string-contains? (fourth (world-letters w)) (world-partial-word w))
   ; contains the main letter
   (ormap (λ (s) (string=? s (world-partial-word w))) DICTIONARY)
   ; partial word is a dictionary word ;;;; make this a helper, same as NEW entered
   (>= (string-length (world-partial-word w)) 4)
   ; length >= 4
   (not (ormap (λ (s) (string=? s (world-partial-word w))) (world-word-log w)))))
; is a NEW entered word

; can-delete? : World KeyEvent -> Boolean
; checks if you hit delete and if there is a letter to
; delete (returns true if you are able to delete)
(check-expect (can-delete? WORLD-1 "\b") #false)
(check-expect (can-delete? (make-world LETTERS-1 "test" (list "tests") 0) "\b") #true)
(define (can-delete? w key)
  (and
   (key=? "\b" key)
   ; if you delete...
   (< 0 (string-length (world-partial-word w)))))
; ...and there is a letter to delete


; can-type? : World KeyEvent -> Boolean
; checks if the entered key is one of the 7 valid letters (reture true if you can type the key)
(check-expect (can-type? WORLD-1 "h") #true)
(check-expect (can-type? (make-world LETTERS-1 "test" (list "tests") 0) "z") #false)
(define (can-type? w key)
  (ormap (λ (l) (key=? key l)) (world-letters w)))
; if you type a valid key


; Scoring:
; 1. One point for a four-letter word,
; 2. An additional point for every additional letter beyond the first four, and
; 3. An additional seven bonus points for using all seven letters.

; change-score : World -> NatNum
; updates the score according to the length of the word
; entered (and if it contains all of the letters)
(check-expect (change-score (make-world LETTERS-1 "test" '() 0)) 1)
(check-expect (change-score (make-world LETTERS-1 "tests" '() 0)) 2)
(check-expect (change-score (make-world LETTERS-1 "hastier" '() 0)) 11)
(check-expect (change-score (make-world LETTERS-1 "set" '() 0)) 0)
(define (change-score w)
  (cond
    [(and
      (= 7 (string-length (world-partial-word w)))
      (andmap (λ (letter) (string-contains? letter (world-partial-word w))) (world-letters w)))
     (+ 11 (world-score w))]
    [(> 4 (string-length (world-partial-word w)))
     (world-score w)]
    [(>= 7 (string-length (world-partial-word w)))
     (+ (world-score w) (- (string-length (world-partial-word w)) 3))]))




(play WORLD-1)