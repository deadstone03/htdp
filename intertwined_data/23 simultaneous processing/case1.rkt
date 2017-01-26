;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname case1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; Exercise 387
; List-of-symbol List-of-number -> list-of-[list-of Symbol Number]
; cross product the two list
(check-expect (cross '(a b) '(1 2)) '((a 1) (a 2) (b 1) (b 2)))
(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else
     (local
       (
        (define (cross-one s lon)
          (cond
            [(empty? lon) '()]
            [else (cons (list s (first lon)) (cross-one s (rest lon)))])))
       (append (cross-one (first los) lon) (cross (rest los) lon)))]))


; Exercise 389
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String).

; List-Of-String List-Of-Number -> List-Of-phone-record
; create a list of phone-record based on a list of name and a list of number
(check-expect (zip (list "a" "b") (list 1 2))
              (list (make-phone-record "a" 1) (make-phone-record "b" 2)))
(define (zip los lon)
  (cond
    [(empty? los) '()]
    [else (cons (make-phone-record (first los) (first lon)) (zip (rest los) (rest lon)))]))


(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 

; TOS List-Of-Directions -> TOS
; get the node in the TOS follow the list of directions

(check-expect (tree-pick (make-branch (make-branch 'a 'b) (make-branch 'c 'd)) (list 'left 'right)) 'b)
(check-error (tree-pick (make-branch (make-branch 'a 'b) (make-branch 'c 'd)) (list 'left 'right 'right))
              "the end of the tree")
(define (tree-pick tos lod)
  (cond
    [(and (symbol? tos) (empty? lod)) tos]
    [(and (symbol? tos) (not (empty? lod))) (error "the end of the tree")]
    [(and (not (symbol? tos)) (empty? lod)) tos]
    [(and (not (symbol? tos)) (not (empty? lod)))
     (if (equal? 'left (first lod))
         (tree-pick (branch-left tos) (rest lod))
         (tree-pick (branch-right tos) (rest lod)))]))