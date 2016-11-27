;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fixed_size_data6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise39
(define UNIT 1)
(define WHEEL-RADIUS (* UNIT 5))
(define CAR-BODY (rectangle (* 40 UNIT) (* 10 UNIT) "solid" "red"))
(define CAR-HEAD (rectangle (* 20 UNIT) (* 5 UNIT) "solid" "red"))
(define CAR-WHEEL (circle WHEEL-RADIUS "solid" "blue"))
(define CAR
  (overlay/xy CAR-WHEEL
              (* -25 UNIT)
              (* -10 UNIT)
              (overlay/xy CAR-WHEEL
                          (* -5 UNIT)
                          (* -10 UNIT)
                          (overlay/xy CAR-HEAD (* -10 UNIT) (* 5 UNIT) CAR-BODY))))