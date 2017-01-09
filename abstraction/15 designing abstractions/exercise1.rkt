;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname exercise1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise250
; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))
  
; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))


; Number -> [List-of Number]
; tabulates f between n and 0(incl.) in a list
(define (tab n f)
  (cond
    [(= n 0) (list (f n))]
    [else
     (cons
      (f n)
      (tab (sub1 n) f))]))

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
;(check-expect (tab-sin-2 3) (list (sin 3) (sin 2) (sin 1) (sin 0)))
(define (tab-sin-2 n)
  (tab n sin))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
; (check-expect (tab-sqrt-2 3) (list (sqrt 3) (sqrt 2) (sqrt 1) (sqrt 0)))
(define (tab-sqrt-2 n)
  (tab n sqrt))


; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))
  
; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; [List-of Number] f init -> Number
; aggregrate the list of number to a number based on f and init
(define (fold1 l f init)
  (cond
    [(empty? l) init]
    [else
     (f (first l) (fold1 (rest l) f init))]))

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(check-expect (sum-2 (list 1 2 3)) 6)
(define (sum-2 l)
  (fold1 l + 0))

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(check-expect (product-2 (list 1 2 3)) 6)
(define (product-2 l)
  (fold1 l * 1))

; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))

(define (image*-2 l)
  (fold1 l place-dot emt))