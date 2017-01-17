;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname computing_with_lambda) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise292

; [X X -> Boolean] -> [ [List-of X] -> Boolean ]
; produces a function that determines whether 
; some list is sorted according to cmp
(define (sorted cmp)
  (lambda (l)
    (cond
      [(empty? l) #true]
      [(empty? (rest l)) #true]
      [else (if (cmp (first l) (first (rest l))) ((sorted cmp) (rest l)) #false)])))

; [X X -> Boolean] [NEList-of X] -> Boolean 
; determine whether l is sorted according to cmp
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
 
(define (sorted? cmp l)
  (cond
    [(empty? l) #true]
    [(empty? (rest l)) #true]
    [else (if (cmp (first l) (first (rest l))) (sorted? cmp (rest l)) #false)]))

(check-expect (sorted?.v2 < '(1 2 3)) #true)
(check-expect (sorted?.v2 < '(2 1 3)) #false)
(define (sorted?.v2 cmp l)
  ((sorted cmp) l))

; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))
; X [List-of X] -> [Number -> boolean]
; generate a function from a i to boolean. if list-of-x's ith item is X, return true, else false
(check-expect ((is-index? 3 (list 1 2 3 4 5)) 2) #true)
(check-expect ((is-index? 3 (list 1 2 3 4 5)) 3) #false)
(check-expect ((is-index? 3 (list 1 2)) 3) #false)
(define (is-index? x l)
  (lambda (i)
    (local
      (
       (define (ith-equal? j al)
         (cond
           [(equal? j 0)
            (cond
              [(empty? al) #false]
              [(equal? x (first al)) #true]
              [else #false])
            ]
           [(empty? al) #false]
           [else (ith-equal? (- j 1) (rest al))])))
      (ith-equal? i l))))