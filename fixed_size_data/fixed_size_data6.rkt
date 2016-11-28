;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fixed_size_data6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise39

(define UNIT 2)
(define WHEEL-RADIUS (* 5 UNIT))
(define CAR-BODY (rectangle (* UNIT 40) (* UNIT 10) "solid" "red"))
(define CAR-HEAD (rectangle (* UNIT 20) (* UNIT 5) "solid" "red"))
(define CAR-WHEEL (circle WHEEL-RADIUS "solid" "blue"))

(define CAR
  (overlay/xy CAR-WHEEL
              (* UNIT -5) (* UNIT -10)
              (overlay/xy CAR-WHEEL
                          (* UNIT -25) (* UNIT -10)
                          (overlay/xy CAR-HEAD
                                      (* UNIT -10) (* UNIT 5)
                                      CAR-BODY))))
(define BACKGROUND (empty-scene (* UNIT 200) (* UNIT 25)))
(define SPEED (* UNIT 3))

; WorldState -> Image
; render the image based on the WorldState ws
(define (render ws)
  (place-image CAR (+ (* UNIT 20) (* UNIT ws)) (* UNIT 15) BACKGROUND))

; WorldState -> WorldState
; moves the car by SPPED pixels for every clock tick
(check-expect (tock 10) 13)
(define (tock ws)
  (+ ws (/ SPEED UNIT)))

; Exercise40
(check-expect (render 0) (place-image CAR 40 30 BACKGROUND))
(check-expect (render 10) (place-image CAR 60 30 BACKGROUND))
(check-expect (render 20) (place-image CAR 80 30 BACKGROUND))

; Exercise41
(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

; WorldState -> boolean
; check if the car is on the right side or not
(check-expect (end? 0) #false)
(check-expect (end? 10) #false)
(check-expect (end? 100) #false)
(check-expect (end? 180) #true)
(check-expect (end? 200) #true)
(define (end? ws)
  (> ws 160))

; Exercise44
; WorldState Number Number String -> WorldState
; places the car at the x-coordinate
; if the given me is "button-down"
(check-expect (hyper 10 20 30 "button-down") 10)
(check-expect (hyper 10 20 30 "button-up") 10)
(define (hyper x-coordinate x-mouse y-mouse me)
  (if (string=? "button-down" me) (/ x-mouse UNIT) x-coordinate))

(define (main ws)
  (big-bang ws
            (on-tick tock)
            (to-draw render)
            (on-mouse hyper)
            (stop-when end?)))

; Exercise42

; Exercise43
; WorldState is the number of tick
; on-tick will be add1

(define BACKGROUND.v2
  (empty-scene (* UNIT 200) (* UNIT 200)))

; WorldState -> Image
; Draw the image based on the WorldState ws
; Moving as a sin.
(define (render.v2 ws)
  (place-image CAR
               (+ (* UNIT 20) (* ws SPEED))
               (+ (* UNIT 100) (* (sin (* ws (/ SPEED UNIT))) UNIT))
               BACKGROUND.v2))
; WorldState -> Boolean
; Check is the car is outside the background or not
(check-expect (end.v2? 0) #false)
(check-expect (end.v2? 53) #false)
(check-expect (end.v2? 54) #true)
(define (end.v2? ws)
  (< (* UNIT 160) (* ws SPEED)))

(define (main.v2 ws)
  (big-bang ws
            (on-tick add1)
            (to-draw render.v2)
            (stop-when end.v2?)))
