;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fixed_size_data8) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise47
; WorldState is the happiness of number
(define MAX-HAPPINESS 100)
(define MIN-HAPPINESS 0)
(define UP-HAPPINESS (/ 1 3))
(define DOWN-HAPPINESS (/ 1 5))
(define BACKGROUND-WIDTH 100)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define HAPPINESS-HEIGHT 10)

; WorldState -> WorldState
; happiness change for every tick
(check-expect (tock 10) 9.9)
(check-expect (tock 0) 0)
(check-expect (tock 0.1) 0)
(define (tock ws)
  (if (>= ws (+ MIN-HAPPINESS 0.1))
      (- ws 0.1)
      ws))

; WorldState String -> WorldState
; WorldState change based on the key
(check-expect (key-handler 0 "up") UP-HAPPINESS)
(check-expect (key-handler 0 "down") DOWN-HAPPINESS)
(check-expect (key-handler MAX-HAPPINESS "up") MAX-HAPPINESS)
(define (key-handler ws key)
  (cond
    [(>= ws MAX-HAPPINESS) ws]
    [(string=? key "up") (+ ws UP-HAPPINESS)]
    [(string=? key "down") (+ ws DOWN-HAPPINESS)]
    [else ws]))

; WorldState -> Image
; render a image based on the WorldState
(check-expect (render 0) BACKGROUND)
(check-expect (render 100)
              (place-image (rectangle 100 HAPPINESS-HEIGHT "solid" "red")
                           50 (/ HAPPINESS-HEIGHT 2)
                           BACKGROUND))
(check-expect (render 50)
              (place-image (rectangle 50 HAPPINESS-HEIGHT "solid" "red")
                           25 (/ HAPPINESS-HEIGHT 2)
                           BACKGROUND))
(define (render ws)
  (place-image (rectangle ws HAPPINESS-HEIGHT "solid" "red")
               (/ ws 2)
               (/ HAPPINESS-HEIGHT 2)
               BACKGROUND))
               

(define (main ws)
  (big-bang ws
            (on-tick tock)
            (on-key key-handler)
            (to-draw render)))