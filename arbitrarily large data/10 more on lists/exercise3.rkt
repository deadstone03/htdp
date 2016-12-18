;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct editor [pre pos])
(define editor1 (make-editor (cons "c" (cons "b" (cons "a" '())))
                             (cons "d" (cons "e" (cons "f" '())))))
; Exercise177
; String String -> Editor
; create a editor from two strings, one for pre, the other for post
(check-expect (create-editor "abc" "def") editor1)
(define (create-editor pre post)
  (make-editor (reverse (explode pre)) (explode post)))

; Exercise178
; Exercise179
; Exercise180