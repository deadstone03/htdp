;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; Exercise 490
; [List-of Number] -> [List-of Number]
; convert a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))
 
; Number [List-of Number] -> [List-of Number]
; add n to each number on l
 
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))
 
(define (add-to-each n l)
  (map (lambda (i) (+ i n)) l))

; Exercise 500
; List-Of-Number -> Number
; product of the list of number
(check-expect (product (list 2 3 4)) 24)
(define (product lon0)
  (local
    (
     ; List-Of-Number Number -> Number
     ; proudct of the list.
     ; accumulator is the product of the numbers before lon.
     (define (product/a lon a)
       (cond
         [(empty? lon) a]
         [else (product/a (rest lon) (* (first lon) a))])))
    (product/a lon0 1)))

; Exercise 501
; List -> Number
; length of the list
(check-expect (how-many (list 1 2 3 4)) 4)
(define (how-many lon)
  (local
    (
     ; List Number -> Number
     ; lenght of the list
     ; accumulator is the length before lon
     (define (how-many/a lon l)
       (cond
         [(empty? lon) l]
         [else (how-many/a (rest lon) (+ 1 l))])))
    (how-many/a lon 0)))

; Exercise 503
; [NEList-of 1String] -> [NEList-of 1String]
; create a palindrome from s0
(check-expect
  (mirror (explode "abc")) (explode "abcba"))
(define (mirror s0)
  (local
    (
     ; [NEList-of 1String] List-of-String -> [NEList-of 1String]
     ; palindrome of s
     ; accumulator is the palindrome for the 1String before s
     (define (mirror/a s a)
       (cond
         [(empty? (rest s)) (cons (first s) a)]
         [else (cons (first s) (mirror/a (rest s) (cons (first s) a)))])))
    (mirror/a s0 '())))

; Exercise 507
; [x -> y] [List-of x] -> [List-of Y]
; mapping every x in the list to y
(check-expect (mymap add1 (list 1 2 3 4)) (list 2 3 4 5))
(define (mymap f lox0)
  (local
    (
     (define (mymap/a lox a)
       (cond
         [(empty? lox) a]
         [else (mymap/a (rest lox) (cons (f (first lox)) a))])))
    (mymap/a (reverse lox0) '())))