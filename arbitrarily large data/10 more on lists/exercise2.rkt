;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise171
; List-of-Strings
;  '()
;  cons(string, List-of-Strings)

; List-of-list-of-strings
;   '()
;   cons(List-of-Strings, List-of-list-of-strings)

; String -> List-of-strings
; split a string on \n
(check-expect (split-n "abc\ndef\n") (cons "abc" (cons "def" (cons "" '()))))
(check-expect (split-n "abc\ndef") (cons "abc" (cons "def" '())))
(define (split-n s)
  (do-split-n s 0))
(define (do-split-n s i)
  (cond
    [(= i (string-length s)) (cons "" '())]
    [(string=? "\n" (string-ith s i)) (cons "" (do-split-n s (+ i 1)))]
    [else (cons (string-append (string-ith s i) (first (do-split-n s (+ i 1))))
                (rest (do-split-n s (+ i 1))))]))

; Exercise172
; Exercise173
; Exercise174
; Exercise175
; Matrix -> Matrix
; transpose the given matrix along the diagonal 
 
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
(define mat1 (cons (cons 11 (cons 12 '())) (cons (cons 21 (cons 22 '())) '()))) 
(check-expect (transpose mat1) tam1)
 
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

(define (first* lln)
  (cond
    [(empty? lln) '()]
    [(empty? (first lln)) '()]
    [else (cons (first (first lln)) (first* (rest lln)))]))

(define (rest* lln)
  (cond
    [(empty? lln) '()]
    [(empty? (first lln)) '()]
    [else (cons (rest (first lln)) (rest* (rest lln)))]))