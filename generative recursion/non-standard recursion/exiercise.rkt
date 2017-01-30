;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exiercise) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keep the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; remove the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

; Exercise 421
; no it will not work.

; Exercise 422
; List-Of-Item Number -> List-Of-List-Of item
; bundle the list of items in a list of list item.
(check-expect (list->chunks (list 1 2 3 4 5) 2) (list (list 1 2) (list 3 4) (list 5)))
(define (list->chunks loi n)
  (local
    (
     ; list-of-item n -> list-of-item
     ; get the first n items of the list
     (define (take loi n)
       (cond
         [(empty? loi) '()]
         [(= n 0) '()]
         [else (cons (first loi) (take (rest loi) (- n 1)))]))
     ; list-of-item n -> list-of-item
     ; remove the first n item of the list
     (define (drop loi n)
       (cond
         [(empty? loi) '()]
         [(= n 0) loi]
         [else (drop (rest loi) (- n 1))])))
    (cond
      [(empty? loi) '()]
      [else (cons (take loi n) (list->chunks (drop loi n) n))])))

; Exercise 422
; string n -> list-of-string
; pase the string to a list of string, every string's length is n
(check-expect (partition "abcde" 2) (list "ab" "cd" "e"))
(define (partition s n)
  (local
    (
     ; string n -> string
     ; get the first n items of the list
     (define (take s n)
       (cond
         [(<= (string-length s) n) s]
         [else (substring s 0 n)]))
     ; string n -> string
     ; remove the first n item of the list
     (define (drop s n)
       (cond
         [(<= (string-length s) n) ""]
         [else (substring s n (string-length s))])))
    (cond
      [(string=? "" s) '()]
      [else (cons (take s n) (partition (drop s n) n))])))