;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fixed_size_data21) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise115
(define MESSAGE "traffic light expected, given: some other value")
(define (light? x)
  (cond
    [(string? x) (or (string=? x "red")
                     (string=? x "yellow")
                     (string=? x "green"))]
    [else #false]))

(define MESSAGE1 "a-value is not a light")
(define MESSAGE2 "another-value is not a light")
(define (light=? a-value another-value)
  (if (light? a-value)
      (if (light? another-value)
          (string=? a-value another-value)
          (error MESSAGE2))
      (error MESSAGE1)))