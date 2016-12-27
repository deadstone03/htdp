;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define DICTIONARY-LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))
(length DICTIONARY-AS-LIST)

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Exercise195
; Letter String -> Boolean
; the string is begin with the letter
(check-expect (starts-with? "a" "apple") #true)
(check-expect (starts-with? "b" "apple") #false)
(define (starts-with? l w)
  (string=? l (substring w 0 1)))

; Letter Dictionary -> Number
; count the number of words start with the letter in the dictionary
(check-expect (starts-with# "a" '()) 0)
(check-expect (starts-with# "a" (list "watermelon" "apple" "banana")) 1)
(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [else (+ (if (starts-with? l (first d)) 1 0)
             (starts-with# l (rest d)))]))

; Exercise 196
; LC is letter and its count
; l is the letter, c is the letter's count
;   (make-lc "a" 20)
(define-struct lc [l c])

; List-of-letter Dictionary-as-list -> list-of-lc
; count the number of words in the dictionary which start with the corresponding letter.
(check-expect (count-by-letter (list "a" "b") (list "apple" "banana"))
              (list (make-lc "a" 1) (make-lc "b" 1)))
(check-expect (count-by-letter '() (list "apple" "banana"))
              '())
(define (count-by-letter lof d)
  (cond
    [(empty? lof) '()]
    [else (cons (make-lc (first lof) (starts-with# (first lof) d))
                (count-by-letter (rest lof) d))]))
;(count-by-letter LETTERS DICTIONARY-AS-LIST)

; Exercise 197
; lc lc -> lc
; get the max count one
(check-expect (max-lc (make-lc "a" 10) (make-lc "b" 20)) (make-lc "b" 20))
(check-expect (max-lc (make-lc "a" 20) (make-lc "a" 10)) (make-lc "a" 20))
(define (max-lc lc1 lc2)
  (if (> (lc-c lc1) (lc-c lc2)) lc1 lc2))

; list-of-lc -> lc
; get the max count lc
(check-expect (most-frequent-lc (list (make-lc "a" 10))) (make-lc "a" 10))
(check-expect (most-frequent-lc (list (make-lc "a" 10) (make-lc "b" 20))) (make-lc "b" 20))
(define (most-frequent-lc lolc)
  (cond
    [(empty? (rest lolc)) (first lolc)]
    [else (max-lc (first lolc) (most-frequent-lc (rest lolc)))]))
; Dictionary -> lc
; get the max frequent first letter
(check-expect (most-frequent (list "apple" "banana" "age"))
              (make-lc "a" 2))
(define (most-frequent d)
  (most-frequent-lc (count-by-letter LETTERS d)))

; Exercise 198

; Letter Dictionary -> List-of-words
; get the words in the dictionary that start with the letter
(check-expect (words-start-with "a" (list "apple" "banana" "age"))
              (list "apple" "age"))
(check-expect (words-start-with "a" '())
              '())
(define (words-start-with l d)
  (cond
    [(empty? d) '()]
    [else (if (starts-with? l (first d))
              (cons (first d) (words-start-with l (rest d)))
              (words-start-with l (rest d)))]))

; list-of-words list-of-list-of-words -> list-of-list-of-words
; add the list-of-words to the head of the list-of-list-of-words if the list-of-words is not empty
(check-expect (add-non-empty (list "a" "b") (list (list "c" "d") (list "e" "f"))) (list (list "a" "b") (list "c" "d") (list "e" "f")))
(check-expect (add-non-empty '() (list (list "c" "d") (list "e" "f"))) (list (list "c" "d") (list "e" "f")))
(define (add-non-empty low lolow)
  (cond
    [(empty? low) lolow]
    [else (cons low lolow)]))
; Letters Dictionary -> list-of-dictionary
; parse the dictionary based on the letters
(check-expect (do-words-by-first-letter (list "a" "b") (list "apple" "banana" "age"))
              (list (list "apple" "age")
                    (list "banana")))
(define (do-words-by-first-letter lol d)
  (cond
    [(empty? lol) '()]
    [else (add-non-empty (words-start-with (first lol) d)
                         (do-words-by-first-letter (rest lol) d))]))

; words-by-first-letter
; Dictionary -> List-of-dictionary
; parse the dictionary based on it's first letters
(check-expect (words-by-first-letter (list "apple" "banana" "age"))
              (list (list "apple" "age")
                    (list "banana")))
(define (words-by-first-letter d)
  (do-words-by-first-letter LETTERS d))

; list-of-words list-of-words -> list-of-words
; get the longer list-of-words
(check-expect (max-list-of-words (list "a" "a") (list "b" "b" "b"))
              (list "b" "b" "b"))
(define (max-list-of-words low1 low2)
  (if (> (length low1) (length low2)) low1 low2))

; list-of-dictionary -> list-of-words
; get the max frequent first letter's words
(check-expect (recur-most-frequent (list (list "apple" "age") (list "banana")))
              (list "apple" "age"))
(define (recur-most-frequent lod)
  (cond
    [(empty? (rest lod)) (first lod)]
    [else (max-list-of-words (first lod) (recur-most-frequent (rest lod)))]))

; list-of-words -> lc
; transform a list of words to letter and count
(check-expect (list-of-words-to-lc (list "apple" "age"))
              (make-lc "a" 2))
(define (list-of-words-to-lc low)
  (make-lc (substring (first low) 0 1) (length low)))

; Dictionary -> lc
; get the max frequent first letter
(check-expect (most-frequent.v2 (list "apple" "banana" "age"))
              (make-lc "a" 2))
(define (most-frequent.v2 d)
  (list-of-words-to-lc (recur-most-frequent (words-by-first-letter d))))

(check-expect (most-frequent.v2 DICTIONARY-AS-LIST)
              (most-frequent DICTIONARY-AS-LIST))