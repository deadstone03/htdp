;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname selfreferential2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise143
; Exercise144
; Exercise145
; NEList-of-temperature -> boolean
; check if the NEList-of-temperature is sorted or not
(check-expect (sorted>? (cons 1 '())) #true)
(check-expect (sorted>? (cons 2 (cons 1 '()))) #true)
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(define (sorted>? alot)
  (cond
    [(empty? (rest alot)) #true]
    [else (if (< (first alot) (first (rest alot)))
              #false
              (sorted>? (rest alot)))]))

; Exercise146
(check-expect (NE-how-many (cons 1 '())) 1)
(check-expect (NE-how-many (cons 1 (cons 2 '()))) 2)
(define (NE-how-many alot)
  (cond
    [(empty? (rest alot)) 1]
    [else (+ (NE-how-many alot) 1)]))

; Exercise147
; NEList-of-Booleans
; (cons boolean '())
; (cons boolean NEList-of-Booleans

;Exercise148
