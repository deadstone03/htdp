;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname projects_abstraction) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise275
(define DIC (list "apple" "banana" "ago" "big" "among"))
; letter count
; (make-lc l c)
; l is the letter, c is the count
(define-struct lc [l c])

; dic -> lc
; get the most frequent letter and its count
(check-expect (most-frequent DIC) (make-lc "a" 3))
(define (most-frequent d)
  (local
    (
     ; dic -> lolc
     ; count dic with word's start letter
     (define dic->lolc
       (local
         (
          (define (count w lolc)
            (local
              (
               (define first-letter (substring w 0 1)))
              (cond
                [(empty? lolc) (cons (make-lc first-letter 1) '())]
                [(string=? first-letter (lc-l (first lolc)))
                 (cons (make-lc (lc-l (first lolc)) (+ (lc-c (first lolc)) 1)) (rest lolc))]
                [else (cons (first lolc) (count w (rest lolc)))])))
          )
         (foldr count '() d)))
     ; list-of-lc -> lc
     ; get the max letter count
     (define (max-lc lolc)
       (local
         (
          ; lc lc -> lc
          ; get the larger lc
          (define (compare-lc lc1 lc2)
            (if (> (lc-c lc1) (lc-c lc2)) lc1 lc2)))
         (foldr compare-lc (first lolc) lolc)))
     )
    (max-lc dic->lolc)))