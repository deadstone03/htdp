;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; A FT is one of: 
; – NP
; – (make-child FT FT String N String)
; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> Boolean
; does a-ftree contain a child
; structure with "blue" in the eyes field
 
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
 
(define (blue-eyed-child? a-ftree)
  (cond
    [(no-parent? a-ftree) #false]
    [else (or (string=? (child-eyes a-ftree) "blue")
              (blue-eyed-child? (child-father a-ftree))
              (blue-eyed-child? (child-mother a-ftree)))]))

; Exercise310
; FT -> Number
; count the number of child in the tree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)
(define (count-persons a-tree)
  (cond
    [(no-parent? a-tree) 0]
    [else (+ 1 (count-persons (child-father a-tree)) (count-persons (child-mother a-tree)))]))


; Exercise311
; FT -> Number
; the average age of the tree
(check-expect (average-age Carl) 74)
(check-expect (average-age Gustav) 45.8)
(define (average-age a-tree)
  (local
    (
     (define (get-age y)
       (- 2000 y))
     (define (sum-age a-tree)
       (cond
         [(no-parent? a-tree) 0]
         [else (+ (get-age (child-date a-tree)) (sum-age (child-father a-tree)) (sum-age (child-mother a-tree)))])))
    (/ (sum-age a-tree) (count-persons a-tree))))

; Exercise312
; FT -> List-of-strings
; get the eye colors of the tree
(check-expect (eye-colors Carl) (list "green"))
(check-expect (eye-colors Gustav) (list "brown" "pink" "blue" "green" "green"))
(define (eye-colors a-tree)
  (cond
    [(no-parent? a-tree) '()]
    [else (append (list (child-eyes a-tree))
                  (eye-colors (child-father a-tree))
                  (eye-colors (child-mother a-tree)))]))

; Exercise313
; FT -> Boolean
; the person's ancestor has blue eyes
(check-expect (blue-eyed-ancestor? Carl) #false)
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(define (blue-eyed-ancestor? a-tree)
  (cond
    [(no-parent? a-tree) #false]
    [else (or (blue-eyed-child? (child-father a-tree)) (blue-eyed-child? (child-mother a-tree)))]))

; Exercise314
; FF -> boolean
; check if any of the persons is blue eye
(check-expect (blue-eyed-child-in-forest? (list Carl Gustav)) #true)
(define (blue-eyed-child-in-forest? ff)
  (ormap blue-eyed-child? ff))

; FF y -> Number
; average age of the FF
(check-expect (average-age-ff (list Carl) 2000) 74)
(define (average-age-ff ff y)
  (local
    (
     (define (get-age d)
       (- y d))
     (define (sum-age a-tree)
       (cond
         [(no-parent? a-tree) 0]
         [else (+ (get-age (child-date a-tree)) (sum-age (child-father a-tree)) (sum-age (child-mother a-tree)))]))
     )
    (/ (foldr (lambda (a-tree s) (+ (sum-age a-tree) s)) 0 ff)
       (foldr (lambda (a-tree s) (+ (count-persons a-tree) s)) 0 ff))))