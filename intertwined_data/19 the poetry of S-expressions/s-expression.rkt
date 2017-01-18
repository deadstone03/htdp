;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s-expression) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise316
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)

; An Atom is one of: 
; – Number
; – String
; – Symbol 

; item -> boolean
; item is an atom or not
(check-expect (atom? 1) #true)
(check-expect (atom? 'a) #true)
(check-expect (atom? "string") #true)
(check-expect (atom? #true) #false)
(define (atom? a)
  (or (number? a) (string? a) (symbol? a)))

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
 (cond
   [(atom? sexp) (count-atom sexp sy)]
   [else (count-sl sexp sy)]))
 
; SL Symbol -> N 
; counts all occurrences of sy in sl 
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else
     (+ (count (first sl) sy) (count-sl (rest sl) sy))]))
 
; Atom Symbol -> N 
; counts all occurrences of sy in at 
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))

; Exercise317
; Exercise318

; S-expr -> Number
; get the depth of the S-expr
(check-expect (depth 'a) 1)
(check-expect (depth '((a b c) (a b c (d e)))) 4)
(check-expect (depth '(a)) 2)
(define (depth sexp)
  (cond
    [(atom? sexp) (depth-atom sexp)]
    [else (+ 1 (depth-ls sexp))]))

; LS -> Number
; get the list of S-expr depth
(check-expect (depth-ls '(a)) 1)
(check-expect (depth-ls '((a b c) (a b c (d e)))) 3)
(define (depth-ls ls)
  (cond
    [(empty? ls) 0]
    [else (max (depth (first ls)) (depth-ls (rest ls)))]))

; Atom -> Number
; get the atom's depth
(check-expect (depth-atom 'a) 1)
(check-expect (depth-atom "as") 1)
(check-expect (depth-atom 1) 1)
(define (depth-atom a)
  1)

; Exercise319
; S-expr Symbol Symbol -> S-expr
; substitute symbol a to symbol b
(check-expect (substitute '((a b c) (a c (a d))) 'a 'b) '((b b c) (b c (b d))))
(define (substitute sexp a b)
  (cond
    [(atom? sexp) (substitute-atom sexp a b)]
    [else (substitute-ls sexp a b)]))

; LS Symbol Symbol -> LS
; substitute symbol a to symbol b
(check-expect (substitute-ls '((a b c) (a c (a d))) 'a 'b) '((b b c) (b c (b d))))
(define (substitute-ls ls a b)
  (cond
    [(empty? ls) '()]
    [else (cons (substitute (first ls) a b) (substitute-ls (rest ls) a b))]))

; Atom Symbol Symbol -> Atom
; if atom is a, replace b, otherwise return atom
(check-expect (substitute-atom 'a 'a 'b) 'b)
(check-expect (substitute-atom 1 'a 'b) 1)
(define (substitute-atom atom a b)
  (if (equal? atom a) b atom))

; S-expr is
;  number
;  symbol
;  string
;  List-of-S-expr
; List-of-S-expr is
;  '()
;  (cons S-expr List-of-S-expr)
(check-expect (count2 '((a b c) (a c (a d))) 'a) 3)
(define (count2 sexp s)
  (cond
    [(number? sexp) 0]
    [(symbol? sexp) (if (equal? sexp s) 1 0)]
    [(string? sexp) 0]
    [(empty? sexp) 0]
    [else (+ (count2 (first sexp) s) (count2 (rest sexp) s))]))