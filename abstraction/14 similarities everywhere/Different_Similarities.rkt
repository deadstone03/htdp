;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Different_Similarities) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise238
(define list1 (list 6 5 4 3 2 1))
(define list2 (list 1 2 3 4 5 6))

; Nelon r -> Number
; determines the smallest number on l, the smallest is defined based on the function r
(define (inf l r)
  (cond
    [(empty? (rest l)) (first l)]
    [else (if (r (first l) (inf (rest l) r)) (first l) (inf (rest l) r))]))

; Nelon -> Number
; the smallest number on l
(check-expect (inf-1 list1) 1)
(define (inf-1 l)
  (inf l <))

; Nelon -> Number
; the largets number on l
(check-expect (sup-1 list2) 6)
(define (sup-1 l)
  (inf l >))

; Nelon r -> number
; determines the smallest number on l
(define (inf.v2 l r)
  (cond
    [(empty? (rest l)) (first l)]
    [else (r (first l) (inf.v2 (rest l) r))]))

; Nelon -> Number
; the smallest number on l
(check-expect (inf-2 list1) 1)
(define (inf-2 l)
  (inf.v2 l min))

; Nelon -> Number
; the largets number on l
(check-expect (sup-2 list2) 6)
(define (sup-2 l)
  (inf.v2 l max))

; Exercise242
; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? los) #false]
    [else (if (string=? (first los) s) (rest los) (occurs s (rest los)))]))