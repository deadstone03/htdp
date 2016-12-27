;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise2_0) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; String -> Word
; convert s to the chosen word representation 
(check-expect (string->word "abc")
              (list "a" "b" "c"))
(define (string->word s)
  (explode s))
 
; Word -> String
; convert w to a string
(check-expect (word->string (list "a" "b" "c"))
              "abc")
(define (word->string w)
  (implode w))

; list-of-words -> list-of-strings
; convert a list of words to a list of strings
(check-expect (words->strings (list (list "a" "b") (list "c" "d")))
              (list "ab" "cd"))
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low)) (words->strings (rest low)))]))

; Exercise 211
; check-member-of
; String -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))

; A Dictionary is a List-of-strings.
(define DICTIONARY-AS-LIST (list "apple" "banana" "cat" "act" "rat" "art" "tar"))

; String Dictionary -> boolean
; the string is in dictionary or not
(check-expect (in-dictionary? "act" DICTIONARY-AS-LIST) #true)
(check-expect (in-dictionary? "pear" DICTIONARY-AS-LIST) #false)
(define (in-dictionary? w d)
  (cond
    [(empty? d) #false]
    [else (or (string=? w (first d)) (in-dictionary? w (rest d)))]))
; List-of-strings -> List-of-strings
; pick out all those Strings that occur in the dictionary
(check-satisfied (in-dictionary (list "rat" "rta" "art" "atr" "tar" "tra"))
                 all-words-from-rat?)              
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if (in-dictionary? (first los) DICTIONARY-AS-LIST)
              (cons (first los) (in-dictionary (rest los)))
              (in-dictionary (rest los)))]))

; letter left right -> word
; create a word from letter left and right
(check-expect (create-word "a" (list "c" "d") (list "e" "f"))
              (list "d" "c" "a" "e" "f"))
(define (create-word l left right)
  (append (reverse left) (cons l '()) right))

; letter word word -> list-of-words
; insert the letter between the first word and the second word
(check-expect (do-insert-everywhere "a" '() (list "b" "c"))
              (list (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")))
(define (do-insert-everywhere l left right)
  (cond
    [(empty? right) (cons (create-word l left right) '())]
    [else (cons (create-word l left right)
                (do-insert-everywhere l (cons (first right) left) (rest right)))]))

; letter word -> list-of-words
; insert the letter into all positions in the word
(check-expect (insert-everywhere "a" (list "b" "c"))
              (list (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")))
(define (insert-everywhere l w)
  (do-insert-everywhere l '() w))

; letter list-of-words -> list-of-words
; insert the letter into all positions in all words
(check-expect (insert-everywhere/in-all-words "a" '()) '())
(check-expect (insert-everywhere/in-all-words "a" (list (list "b"))) (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere/in-all-words "a" (list (list "b" "c")))
              (list (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")))
(define (insert-everywhere/in-all-words l low)
  (cond
    [(empty? low) '()]
    [else (append (insert-everywhere l (first low))
                  (insert-everywhere/in-all-words l (rest low)))]))