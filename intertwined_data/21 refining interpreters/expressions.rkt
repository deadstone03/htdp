;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname expressions) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define-struct add [left right])
(define-struct mul [left right])

; Expression is
;   Number
;   add
;   mul

; Exercise347
; Expression -> number
; get the value of the expression
(check-expect (eval-expression (make-add (make-mul 1 2) (make-mul 2 3))) 8)
(define (eval-expression e)
  (cond
    [(number? e) e]
    [(add? e) (+ (eval-expression (add-left e)) (eval-expression (add-right e)))]
    [else (* (eval-expression (mul-left e)) (eval-expression (mul-right e)))]))

; Exercise348
(define-struct myand [left right])
(define-struct myor [left right])
(define-struct mynot [value])

; b-experssion is
;  boolean
;  myand
;  myor
;  mynot
(check-expect (eval-bool-expression (make-myand (make-myor (make-mynot #true) #false) (make-mynot #false)))
              #false)
(define (eval-bool-expression e)
  (cond
    [(boolean? e) e]
    [(mynot? e) (not (eval-bool-expression (mynot-value e)))]
    [(myand? e) (and (eval-bool-expression (myand-left e)) (eval-bool-expression (myand-right e)))]
    [else (or (eval-bool-expression (myor-left e)) (eval-bool-expression (myor-right e)))]))

(define (atom? e)
  (or (number? e) (symbol? e) (string? e)))
(define WRONG "wrong")
; S-expr -> BSL-expr
(check-expect (parse '(+ (* 2 3) (* 4 5)))
              (make-add (make-mul 2 3) (make-mul 4 5)))
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

; an S-expr -> Number
; get the value of the S-expr
(check-expect (interpreter-expr '(+ (* 2 3) (* 4 5)))
              26)
(define (interpreter-expr e)
  (eval-expression (parse e)))


; Exercise 352
; BSL-expr Symbol Number -> BSL-expr
(check-expect (subst (make-add (make-mul 1 2) (make-mul 'x 3)) 'x 4)
              (make-add (make-mul 1 2) (make-mul 4 3)))
(define (subst e s n)
  (cond
    [(atom? e) (if (equal? e s) n e)]
    [(mul? e) (make-mul (subst (mul-left e) s n) (subst (mul-right e) s n))]
    [else (make-add (subst (add-left e) s n) (subst (add-right e) s n))]))

; Exercise 353
; BSL-var-expr -> Boolean
; check if the e has symbol or not
(check-expect (numeric? (make-add (make-mul 1 2) (make-mul 2 3))) #true)
(check-error (numeric? (make-add (make-mul 1 2) (make-mul 'x 3))) "Not numerical")
(define (numeric? e)
  (cond
    [(atom? e) (if (symbol? e) (error "Not numerical") #true)]
    [(mul? e) (and (numeric? (mul-left e)) (numeric? (mul-right e)))]
    [else (and (numeric? (add-left e)) (numeric? (add-right e)))]))

;Exercise 354
; BSL-val-expr -> Number
(check-expect (eval-variable (make-add (make-mul 1 2) (make-mul 4 3))) 14)
(check-error (eval-variable (make-add (make-mul 1 2) (make-mul 'x 3))) "Not numerical")
(define (eval-variable e)
  (if (numeric? e) (eval-expression e) #false))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; BSL-val-expr List-Of-AL -> Number
; evaluate the value of the bsl val expr
(check-expect (eval-variable* (make-add (make-mul 1 2) (make-mul 'x 3)) (list (list 'x 4))) 14)
(define (eval-variable* e loal)
  (local
    (
     (define (subst* e loal)
       (foldr (lambda (al e) (subst e (first al) (second al))) e loal))
     (define substed-e (subst* e loal)))
    (if (numeric? substed-e) (eval-expression substed-e) #false)))


; BSL-var-expr AL -> Number
(check-expect (eval-var-lookup (make-add (make-mul 1 2) (make-mul 'x 3)) (list (list 'x 4))) 14)
(check-error (eval-var-lookup (make-add (make-mul 'y 2) (make-mul 'x 3)) (list (list 'x 4))) "y is not defined")
(define (eval-var-lookup e da)
  (local
    (
     (define (lookup e da)
       (cond
         [(empty? da) (error (string-append (symbol->string e) " is not defined"))]
         [else (if (equal? e (first (first da))) (second (first da)) (lookup e (rest da)))])))
    (cond
      [(number? e) e]
      [(symbol? e) (lookup e da)]
      [(string? e) (error (string-append e " is not a number"))]
      [(mul? e) (* (eval-var-lookup (mul-left e) da) (eval-var-lookup (mul-right e) da))]
      [(add? e) (+ (eval-var-lookup (add-left e) da) (eval-var-lookup (add-right e)  da))])))
