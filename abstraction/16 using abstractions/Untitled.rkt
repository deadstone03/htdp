;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise 267
; list-of-number -> list-of-number
; convert euro to us dollar
(check-expect (convert-euro (list 1 2 3)) (list 1.22 2.44 3.66))
(define (convert-euro loe)
  (local
    (; number -> number
     ; conver euro to us dollar
     (define (convert-euro-1 e)
       (* 1.22 e)))
    (map convert-euro-1 loe)))

; list-of-posn -> list-of-list-of-number
; translate a list of posns to a list of list of numbers
(check-expect (translate (list (make-posn 1 2) (make-posn 3 4))) (list (list 1 2) (list 3 4)))
(define (translate lop)
  (local
    (
     (define (translate-1 p)
       (list (posn-x p) (posn-y p))))
  (map translate-1 lop)))

; Exercise 268
; Exercise 269
; Exercise 270
; [x] number [number -> x] -> [list-of x]
; produce n items, each item is produced from f(i)
(check-expect (make-list.v2 3 add1) (list 4 3 2))
(define (make-list.v2 n f)
  (cond
    [(= n 0) '()]
    [else (cons (f n) (make-list.v2 (- n 1) f))]))

(check-expect (build-list-1 3) (list 0 1 2))
(define (build-list-1 n)
  (local
    (
     (define (build-list-1-1 i)
       (- n i)))
    (make-list.v2 n build-list-1-1)))

(check-expect (build-list-4 3) (list 0 2 4))
(define (build-list-4 n)
  (local
    (
     (define (build-list-4-1 i)
       (* (- n i) 2)))
    (make-list.v2 n build-list-4-1)))

; Exercise273
(check-expect (map.v2 (list 1 2 3) add1) (list 2 3 4))
(define (map.v2 l f)
  (local
    (
     (define (map-1 i pl)
       (cons (f i) pl)))
    (foldr map-1 '() l)))
    