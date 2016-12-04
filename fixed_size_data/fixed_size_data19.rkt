;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fixed_size_data19) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise94

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; A UFO is a Posn.
;(using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure
;  (make-tank Number Number)
; (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick
; A Missile is a Posn
; (make-posn x y) is the missile's place

; A SIGS is one of:
; - (make-aim UFO Tank)
; - (make-fired UFO Tank Missile)
; represents the complete state of a space invader game

(make-aim (make-posn 20 10) (make-tank 20 -3))
(make-fired (make-posn 20 10) (make-tank 20 -3) (make-posn 20 20))

; Exercise95
; Exercise96
(define WIDTH 200)
(define HEIGHT 200)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define MISSILE (rectangle 5 10 "solid" "red"))
(define TANK (rectangle 20 5 "solid" "blue"))
(define UFO (overlay (circle 5 "solid" "green") (rectangle 20 5 "solid" "green")))
(define TANK-Y (- HEIGHT (/ (image-height TANK) 2)))
(define UFO-MAX-Y (- HEIGHT (/ (image-height UFO) 2)))

; Tank Image -> Image
; add Tank to the image
(check-expect (tank-render (make-tank 10 3) BACKGROUND) (place-image TANK 10 TANK-Y BACKGROUND))
(define (tank-render t img)
  (place-image TANK (tank-loc t) TANK-Y img))

; UFO Image -> Image
; add UFO to the image
(check-expect (ufo-render (make-posn 20 30) BACKGROUND) (place-image UFO 20 30 BACKGROUND))
(define (ufo-render u img)
  (place-image UFO (posn-x u) (posn-y u) img))

; Missile Image -> Image
; add Missile to image
(check-expect (missile-render (make-posn 20 30) BACKGROUND) (place-image MISSILE 20 30 BACKGROUND))
(define (missile-render m img)
  (place-image MISSILE (posn-x m) (posn-y m) img))

; SIGS -> Image
; adds TANK, UFO and possibly MISSILE to
; the BACKGROUND scene
(define (si-render s)
  (cond
    [(aim? s) (tank-render (aim-tank s) (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s) (missile-render (fired-missile s) (tank-render (fired-tank s) (ufo-render (fired-ufo s) BACKGROUND)))]))

; Exercise97

; UFO -> Boolean
; check if UFO landed or not
(check-expect (ufo-landed? (make-posn 10 UFO-MAX-Y)) #true)
(check-expect (ufo-landed? (make-posn 0 0)) #false)
(define (ufo-landed? u)
   (>= (posn-y u) UFO-MAX-Y))

; UFO Missile -> Boolean
; check if Missile destoryed UFO or not
(check-expect (destory? (make-posn 10 20) (make-posn 10 100)) #false)
(check-expect (destory? (make-posn 10 20) (make-posn 10 20)) #true)
(define (destory? u m)
  (and (< (abs (- (posn-x u) (posn-x m))) (/ (+ (image-width UFO) (image-width MISSILE)) 2))
       (< (abs (- (posn-y u) (posn-y m))) (/ (+ (image-height UFO) (image-width MISSILE)) 2))))

; SIGS -> Boolean
; check if the game finished or not
(check-expect (si-game-over? (make-aim (make-posn 10 UFO-MAX-Y) (make-tank 10 3))) #true)
(check-expect (si-game-over? (make-aim (make-posn 10 20) (make-tank 10 3))) #false)
(check-expect (si-game-over? (make-fired (make-posn 10 UFO-MAX-Y) (make-tank 10 3) (make-posn 10 20))) #true)
(check-expect (si-game-over? (make-fired (make-posn 10 20) (make-tank 10 3) (make-posn 10 20))) #true)
(check-expect (si-game-over? (make-fired (make-posn 10 20) (make-tank 10 3) (make-posn 10 100))) #false)
(define (si-game-over? s)
  (cond
    [(aim? s) (ufo-landed? (aim-ufo s))]
    [(fired? s) (or (ufo-landed? (fired-ufo s)) (destory? (fired-ufo s) (fired-missile s)))]))

; SIGS -> Image
; render the final status
(define (si-render-final s)
  (cond
    [(aim? s) (si-render s)]
    [(fired? s) (if (destory? (fired-ufo s) (fired-missile s))
                    (tank-render (fired-tank s) BACKGROUND) (si-render s))]))

; Exercise99
(define 
