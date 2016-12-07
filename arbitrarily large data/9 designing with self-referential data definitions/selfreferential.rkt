;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname selfreferential) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise137
; Exercise138
; List-of-amounts -> Number
; Get the sum of the list
(check-expect (my-sum '()) 0)
(check-expect (my-sum (cons 10 '())) 10)
(check-expect (my-sum (cons 10 (cons 20 '()))) 30)
(define (my-sum loa)
  (cond
    [(empty? loa) 0]
    [else (+ (first loa) (my-sum (rest loa)))]))

; Exercise139

; List-of-boolean -> boolean
; If all true return true, otherwise return false
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #false (cons #true '()))) #false)
(define (all-true aob)
  (cond
    [(empty? aob) #true]
    [else (if (first aob) (all-true (rest aob)) #false)]))

; List-of-boolean -> boolean
; if one is true return true, otherwise return false
(check-expect (one-true '()) #false)
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false (cons #true '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(define (one-true aob)
  (cond
    [(empty? aob) #false]
    [else (if (first aob) #true (one-true (rest aob)))]))

; Exercise141
; List-of-string -> string
; concatenate all strings in l into one long string
(check-expect (cat '()) "")
(check-expect (cat (cons "a" '())) "a")
(check-expect (cat (cons "a" (cons "b" (cons "c" '())))) "abc")
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))

; Exercise142
; List-of-image n -> ImageOrFalse
; return the first image whose size is n*n. If no such image, return #false
(check-expect (ill-sized? '() 10) #false)
(check-expect (ill-sized? (cons (circle 5 "solid" "red") '()) 10) (circle 5 "solid" "red"))
(check-expect (ill-sized? (cons (circle 6 "solid" "red") '()) 10) #false)
(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (if (and (= (image-width (first loi)) n) (= (image-height (first loi)) n))
              (first loi)
              (ill-sized? (rest loi) n))]))