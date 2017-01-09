;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname exercise1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise235
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Los -> boolean
; whether l contains string atom or not
(check-expect (contain-atom? (list "atom")) #true)
(check-expect (contain-atom? (list "btom")) #false)
(define (contain-atom? l)
  (contains? "atom" l))

; Number Lon -> Lon
; add the number to every element in the lon
(check-expect (add-all 1 (list 1 2 3)) (list 2 3 4))
(check-expect (add-all -1 (list 1 2 3)) (list 0 1 2))
(define (add-all n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ n (first l)) (add-all n (rest l)))]))