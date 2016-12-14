;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise166
; Exercise167
; ListOfPosn -> Number
; get the sum of all x
(check-expect (sum-x '()) 0)
(check-expect (sum-x (cons (make-posn 20 10) '())) 20)
(check-expect (sum-x (cons (make-posn 20 10) (cons (make-posn 10 20) '()))) 30)
(define (sum-x lop)
  (cond
    [(empty? lop) 0]
    [else (+ (posn-x (first lop)) (sum-x (rest lop)))]))
; Exercise168
; Exercise169
; Exercise170
