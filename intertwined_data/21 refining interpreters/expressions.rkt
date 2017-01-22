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

; Exercise356
(define-struct func [name arg])
; (k (+ 1 1))
; (make-func 'k (make-add 1 1))
; (* 5 (k (+ 1 1)))
; (make-mul 5 (make-func k (make-add 1 1)))
; (* (i 5) (k (+ 1 1)))
; (make-mul (make-func i 5) (make-func k (make-add 1 1)))

; A BSL-func-expr is one of: 
; – Number
; – Symbol
; – (make-add BSL-func-expr BSL-func-expr)
; – (make-mul BSL-func-expr BSL-func-expr)
; - (make-func name BSL-func-expr)

; BSL-func-expr Symbol Symbol BSL-func-expr -> Number
; evaluate a func expression
(check-expect (eval-definition1 (make-func 'k 5) 'k 'x (make-add 'x 1)) 6)
(check-expect (eval-definition1 (make-func 'k (make-func 'k 5)) 'k 'x (make-add 'x 1)) 7)
(check-expect (eval-definition1 (make-func 'k (make-add 1 1)) 'k 'x (make-add 'x 1)) 3)
(check-expect (eval-definition1 (make-mul (make-func 'k 5) (make-func 'k (make-add 1 1)))
                                'k 'x (make-add 'x 1))
              18)
(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(func? ex) (eval-definition1 (subst b x (eval-definition1 (func-arg ex) f x b)) f x b)]
    [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                  (eval-definition1 (add-right ex) f x b))]
    [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                  (eval-definition1 (mul-right ex) f x b))]))

; Exercise 358
(define-struct func-def [name arg body])
; name is a Symbol
; arg is a Symbol
; body is a BSL-func-expr

; (define (f x) (+ 3 x))
; (make-func-def (f x (list '+ 3 'x)))
; (define (g y) (f (* 2 y)))
; (make-func-def (g 'y (list 'f (list '* 2 '7))))
; (define (h v) (+ (f v) (g v)))
; (make-func-def (h 'v (list '+ (list 'f 'v) (list 'g 'v)))

(define f (make-func-def 'f 'x (make-add 3 'x)))
(define g (make-func-def 'g 'y (make-func 'f (make-mul 2 'y))))
(define h (make-func-def 'h 'v (make-add (make-func 'f 'v) (make-func 'g 'v))))

(define da-fgh (list f g h))
; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(define (lookup-def da f)
  (cond
    [(empty? da) (error (string-append (symbol->string f) " is not defined"))]
    [(equal? (func-def-name (first da)) f) (first da)]
    [else (lookup-def (rest da) f)]))

; Exercise 359

; Exercise 352
; BSL-expr Symbol Number -> BSL-expr
(check-expect (subst-func (make-add (make-mul 1 2) (make-mul 'x 3)) 'x 4)
              (make-add (make-mul 1 2) (make-mul 4 3)))
(check-expect (subst-func (make-func 'f (make-mul 'x 3)) 'x 4)
              (make-func 'f (make-mul 4 3)))
(define (subst-func e s n)
  (cond
    [(atom? e) (if (equal? e s) n e)]
    [(mul? e) (make-mul (subst-func (mul-left e) s n) (subst-func (mul-right e) s n))]
    [(add? e) (make-add (subst-func (add-left e) s n) (subst-func (add-right e) s n))]
    [(func? e) (make-func (func-name e) (subst-func (func-arg e) s n))]))

; BSL-fun-expr list-of-function -> Number
; evaluate the expression
(check-expect (eval-function* (make-func 'g (make-mul 2 3)) da-fgh) 15)
(check-expect (eval-function* (make-func 'h 15) da-fgh) 51)
(check-expect (eval-function* (make-func 'f (make-func 'g 2)) da-fgh) 10)
(check-expect (eval-function*
               (make-add (make-func 'f (make-func 'g 2))
                         (make-func 'h (make-func 'g (make-mul 2 3))))
               da-fgh)
              61)
(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(add? ex) (+ (eval-function* (add-left ex) da) (eval-function* (add-right ex) da))]
    [(mul? ex) (* (eval-function* (mul-left ex) da) (eval-function* (mul-right ex) da))]
    [(func? ex) (local
                  (
                   (define f-def (lookup-def da (func-name ex))))
                  (eval-function*
                   (subst-func (func-def-body f-def) (func-def-arg f-def) (eval-function* (func-arg ex) da))
                   da))]))

(define-struct da-all [vs fs])
; vs is a list of (list symbol value)
; fs is a list of func-def

(define (lookup-con da e)
  (cond
    [(empty? da) (error (string-append (symbol->string e) " is not defined"))]
    [else (if (equal? e (first (first da))) (second (first da)) (lookup-con e (rest da)))]))

(define (lookup-con-def da s)
  (lookup-con (da-all-vs da) s))

(define (lookup-fun-def da f)
  (lookup-def (da-all-fs da) f))

eval-all