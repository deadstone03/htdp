;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise181
(define l1 (list "a" "b" "c"))

; Exercise182
; Exercise183
; Exercise184
; Exercise185
(first (list 1 2 3))
(rest (list 1 2 3))
(second (list 1 2 3))
(third (list 1 2 3))
(fifth (list 1 2 3 4 5))

; Exercise186
; Exercise187

(define-struct gp [name score])

; gp gp -> boolean
; is the first gp's score larger than the second one
(check-expect (gp>? (make-gp "a" 10) (make-gp "b" 9)) #true)
(check-expect (gp>? (make-gp "a" 9) (make-gp "b" 10)) #false)
(define (gp>? gp1 gp2)
  (> (gp-score gp1) (gp-score gp2)))

; ListOfGp -> List of gp
; sort the list of gp by score
(check-expect (sort-gp> (list (make-gp "a" 9) (make-gp "b" 8))) (list (make-gp "a" 9) (make-gp "b" 8)))
(check-expect (sort-gp> '()) '())
(define (sort-gp> alogp)
  (cond
    [(empty? alogp) '()]
    [(cons? alogp) (insert-gp (first alogp) (sort-gp> (rest alogp)))]))


; gp Listofgp -> List of gp
; instert the gp into a list of gp.
(check-expect (insert-gp (make-gp "b" 8) (list (make-gp "a" 9))) (list (make-gp "a" 9) (make-gp "b" 8)))
(check-expect (insert-gp (make-gp "b" 10) (list (make-gp "a" 9))) (list (make-gp "b" 10) (make-gp "a" 9)))
(define (insert-gp agp alogp)
  (cond
    [(empty? alogp) (list agp)]
    [(cons? alogp) (if (gp>? agp (first alogp))
                       (cons agp alogp)
                       (cons (first alogp) (insert-gp agp (rest alogp))))]))
; Exercise188
; Exercise189
; Exercise190
; 1Strings -> 1Strings
; delete the last 1string of the 1strings
(check-expect (prefix (list "a" "b" "c")) (list "a" "b"))
(check-expect (prefix (list "a")) '())
(define (prefix ss)
  (cond
    [(empty? (rest ss)) '()]
    [else (cons (first ss) (prefix (rest ss)))]))
; 1Stings -> ListOf1Strings
; Get the prefixes of a 1Strings
(check-expect (prefixes (list "a" "b" "c")) (list (list "a" "b" "c") (list "a" "b") (list "a") '()))
(check-expect (prefixes '()) (cons '() '()))
(define (prefixes ss)
  (cond
    [(empty? ss) (cons '() '())]
    [else (cons ss (prefixes (prefix ss)))]))

; Exercise191
; Exercise192
; Exercise193
