;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bsl1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise116
; Exercise117
; Exercise118
; Exercise119
; Exercise120
; Exercise121
; Exercise122
; Exercise123
; Exercise124
; Exercise125
; Exercise126
; Exercise127
; Exercise128

(check-expect 3 3)
(check-member-of "yellow" "red" "yellow" "grey")
(check-member-of "grey" "red" "yellow" "grey")
(check-member-of "red" "red" "yellow" "grey")

(check-within (make-posn #i1.0 #i1.1)
              (make-posn #i0.9 #i1.2)
              0.1)
(check-range #i0.7 #i0.6 #i0.8)
(check-error (/ 1 0))
(check-random (make-posn (random 3) (random 9))
              (make-posn (random 3) (random 9)))
(check-satisfied 4 even?)