;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sort) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort< (list 3 2 1 4 5)) (list 1 2 3 4 5))
(check-expect (quick-sort< (list 3 2 1 1 3 4 5)) (list 1 1 2 3 3 4 5))
(define (quick-sort< alon)
  (cond
    [(or (empty? alon) (empty? (rest alon))) alon]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers (rest alon) pivot))
                    (list pivot)
                    (quick-sort< (largers (rest alon) pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (filter (lambda (x) (> x n)) alon))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (filter (lambda (x) (<= x n)) alon))

; Exercise 430
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(check-expect (quick-sort<.v2 (list 3 2 1 1 3 4 5)) (list 1 1 2 3 3 4 5))
(define (quick-sort<.v2 alon)
  (cond
    [(or (empty? alon) (empty? (rest alon))) alon]
    [else (local
            (
             (define pivot (first alon))
             ; list-of-number -> list-of-list-of-number
             ; parse the list-of-number to two list-of-number, first larger, second not larger
             (define (parse alon)
               (cond
                 [(empty? alon) (list '() '())]
                 [else (local
                         (
                          (define parsed-rest (parse (rest alon))))
                         (cond
                           [(< (first alon) pivot) (list (cons (first alon) (first parsed-rest)) (second parsed-rest))]
                           [else (list (first parsed-rest) (cons (first alon) (second parsed-rest)))]))]))
             (define parsed-alon (parse (rest alon))))
            (append (quick-sort<.v2 (first parsed-alon))
                    (list pivot)
                    (quick-sort<.v2 (second parsed-alon))))]))