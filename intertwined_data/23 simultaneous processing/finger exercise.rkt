;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |finger exercise|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; Exercise 393
; Set Set -> Set
; union of two sets
; set is a list of numbers;
(check-expect (union (list 1 2 3 4) (list 2 3 4 5)) (list 2 3 4 5 1))
(define (union s1 s2)
  (local
    (
     ; set Number -> set
     ; add one number to the set.
     (define (set-add s n)
       (cond
         [(empty? s) (cons n '())]
         [else (if (equal? n (first s)) s (cons (first s) (set-add (rest s) n)))])))
    (cond
      [(empty? s1) s2]
      [else (set-add (union (rest s1) s2) (first s1))])))

; Set Set -> Set
; intersection of two sets
(check-expect (intersect (list 1 2 3 4) (list 2 3 4 5)) (list 2 3 4))
(define (intersect s1 s2)
  (local
    (
     ; Number Set -> Boolean
     ; check if the number is in the set
     (define (in? n s)
       (cond
         [(empty? s) #false]
         [else (if (equal? (first s) n) #true (in? n (rest s)))])))
    (cond
      [(empty? s1) '()]
      [else (if (in? (first s1) s2)
                (cons (first s1) (intersect (rest s1) s2))
                (intersect (rest s1) s2))])))

; Exercise 394
; List-Of-Number List-Of-Number -> List-Of-Number
; merge two ascend list of number to one ascend list of number
(check-expect (merge (list 1 3 5 7 9) (list 2 3 4 5 6)) (list 1 2 3 3 4 5 5 6 7 9))
(define (merge lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(empty? lon2) lon1]
    [(< (first lon1) (first lon2)) (cons (first lon1) (merge (rest lon1) lon2))]
    [else (cons (first lon2) (merge lon1 (rest lon2)))]))

; Exercise 401
; S-exp S-exp -> Boolean
; check if two S-exp equals or not
(check-expect (sexp=? '(x (1 (2)) y) '(x (1 (2)) y)) #true)
(check-expect (sexp=? '(x (1 (2)) y) '(x (1 (2)) (y z))) #false)
(define (sexp=? s1 s2)
  (cond
    [(or (symbol? s1) (number? s1) (string? s1)) (equal? s1 s2)]
    [(and (empty? s1) (empty? s2)) #true]
    [(and (cons? s1) (cons? s2)) (and (sexp=? (first s1) (first s2)) (sexp=? (rest s1) (rest s2)))]
    [else #false]))