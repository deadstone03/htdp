;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fixed_size_data13) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))
 
; A Posn represents the state of the world.

; Posn -> Image
; adds a red spot to MTS at p
(check-expect (scene+dot (make-posn 10 20))
              (place-image DOT 10 20 MTS))
(check-expect (scene+dot (make-posn 88 73))
              (place-image DOT 88 73 MTS))
(define (scene+dot p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

; Posn -> Posn
; add 3 to x
(check-expect (x+ (make-posn 10 20)) (make-posn 13 20))
(define (x+ ap)
  (make-posn (+ (posn-x ap) 3) (posn-y ap)))

; Posn Number Number MouseEvt -> Posn 
; for mouse clicks, (make-posn x y); otherwise p
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-down")
  (make-posn 29 31))
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-up")
  (make-posn 10 20))
(define (reset-dot p x y me)
  (if (string=? me "button-down") (make-posn x y) p))

; Posn -> Posn 
(define (main p0)
  (big-bang p0
    [on-tick x+]
    [on-mouse reset-dot]
    [to-draw scene+dot]))

; Exercise73
; Posn Number -> Posn
; set the x to Number
(check-expect (posn-up-x (make-posn 10 20) 30) (make-posn 30 20))
(define (posn-up-x ap x)
  (make-posn x (posn-y ap)))
; Exercise74
; Exercise75
; Vel is a velocity
(define-struct vel [deltax deltay])

; UFO has position and velocity
(define-struct ufo [loc vel])

; UFO -> UFO
; move one tick for the UFO
(check-expect (ufo-move-1 (make-ufo (make-posn 10 20) (make-vel -1 -4))) (make-ufo (make-posn 9 16) (make-vel -1 -4)))
(define (ufo-move-1 u)
  (make-ufo (posn+ (ufo-loc u) (ufo-vel u)) (ufo-vel u)))

; Posn Vel -> Posn
; change the posn with velocity
(check-expect (posn+ (make-posn 10 20) (make-vel -1 -4)) (make-posn 9 16))
(define (posn+ loc v)
  (make-posn (+ (posn-x loc) (vel-deltax v)) (+ (posn-y loc) (vel-deltay v))))

; Exercise76
; Exercise77
; time is the time from midnight
;   (make-time (Number Number Number))
(define-struct time [hours minutes seconds])

; Exercise78
; Three letters word
; letter is:
; a-z
; or #false
;   (make-word letter letter letter)
(define-struct word [l1 l2 l3])

; Exercise79
; Exercise80
; Exercise81
; time -> seconds
; Convert the time to seconds
(check-expect (time->seconds (make-time 0 0 10)) 10)
(check-expect (time->seconds (make-time 1 1 1)) (+ (* 60 (+ (* 60 1) 1)) 1))
(define (time->seconds t)
  (+ (* 60 (+ (* 60 (time-hours t)) (time-minutes t))) (time-seconds t)))

; word word -> word
; compare two word
(check-expect (compare-word (make-word "a" "b" "c") (make-word "a" "b" "c")) (make-word "a" "b" "c"))
(check-expect (compare-word (make-word "a" "b" "c") (make-word "d" "e" "f")) (make-word #false #false #false))
(check-expect (compare-word (make-word "a" "b" "c") (make-word "a" "d" "c")) (make-word "a" #false "c"))
(define (compare-word word1 word2)
  (make-word (if (equal? (word-l1 word1) (word-l1 word2)) (word-l1 word1) #false)
             (if (equal? (word-l2 word1) (word-l2 word2)) (word-l2 word1) #false)
             (if (equal? (word-l3 word1) (word-l3 word2)) (word-l3 word1) #false)))

