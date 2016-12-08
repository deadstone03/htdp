;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname selfreferential3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise150

; N -> Number
; computes (+ n pi) without using +
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

; Exercise151
; N number -> Number
; multiply the number by N without use multiply
(check-expect (multiply 10 2) 20)
(define (multiply n num)
  (cond
    [(zero? n) 0]
    [else (+ num (multiply (sub1 n) num))]))

; Exercise152
; N image -> image
; horizontally arrange the image n times
(check-expect (row 0 (circle 5 "solid" "red")) empty-image)
(check-expect (row 2 (circle 5 "solid" "red")) (beside (circle 5 "solid" "red") (circle 5 "solid" "red")))
(define (row n img)
  (cond
    [(zero? n) empty-image]
    [else (beside img (row (sub1 n) img))]))
; N Image -> Image
; vertically arrange the image n times
(check-expect (col 0 (circle 5 "solid" "red")) empty-image)
(check-expect (col 2 (circle 5 "solid" "red")) (above (circle 5 "solid" "red") (circle 5 "solid" "red")))
(define (col n img)
  (cond
    [(zero? n) empty-image]
    [else (above img (col (sub1 n) img))]))

; Exercise153
(define SIZE 10)
(define WIDTH 8)
(define HEIGHT 18)
(define DOT (circle 2 "solid" "red"))
(define MTSN (empty-scene (+ (* SIZE WIDTH) 8) (+ (* SIZE HEIGHT) 8)))
(define GRID (overlay (col HEIGHT (row WIDTH (rectangle SIZE SIZE "outline" "black"))) MTSN))
; List-of-posn image -> Image
; draw red dots on the posns of the image
(define (draw lop img)
  (cond
    [(empty? lop) img]
    [else (place-image DOT (* SIZE (posn-x (first lop))) (* SIZE (posn-y (first lop))) (draw (rest lop) img))]))

; Exercise154
(define-struct layer [color doll])
; RD -> String
; concatinate the RD's colors
(check-expect (rd->color (make-layer "yellow" (make-layer "green" "red"))) "yellow, green, red")
(define (rd->color rd)
  (cond
    [(string? rd) rd]
    [else (string-append (layer-color rd) ", " (rd->color (layer-doll rd)))]))

; RD -> String
; ineral color
(check-expect (rd->last (make-layer "yellow" (make-layer "green" "red"))) "red")
(define (rd->last rd)
  (cond
    [(string? rd) rd]
    [else (rd->last (layer-doll rd))]))