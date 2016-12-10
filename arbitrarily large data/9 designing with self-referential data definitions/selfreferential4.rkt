;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname selfreferential4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise156
; Exercise157
; Exercise158

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missiles])
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
; - (make-fired UFO Tank Missiles)
; represents the complete state of a space invader game

(make-aim (make-posn 20 10) (make-tank 20 -3))
(make-fired (make-posn 20 10) (make-tank 20 -3) (cons (make-posn 20 20) '()))

; Exercise95
; Exercise96
(define WIDTH 200)
(define HEIGHT 400)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define MISSILE (rectangle 5 10 "solid" "red"))
(define TANK (rectangle 20 5 "solid" "blue"))
(define UFO (overlay (circle 5 "solid" "green") (rectangle 20 5 "solid" "green")))
(define TANK-Y (- HEIGHT (/ (image-height TANK) 2)))
(define UFO-MAX-Y (- HEIGHT (/ (image-height UFO) 2)))
(define UFO-SPEED 1)
(define MISSILE-SPEED (* UFO-SPEED 3))

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

; a ListOfMissiles Image -> Image
; add missiles to the image
(define (missiles-render ms img)
  (cond
    [(empty? ms) img]
    [else (missile-render (first ms) (missiles-render (rest ms) img))]))

; SIGS -> Image
; adds TANK, UFO and possibly MISSILE to
; the BACKGROUND scene
(define (si-render s)
  (cond
    [(aim? s) (tank-render (aim-tank s) (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s) (missiles-render (fired-missiles s) (tank-render (fired-tank s) (ufo-render (fired-ufo s) BACKGROUND)))]))

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

; UFO Missiles -> Boolean
; Any missile destroy the UFO
(check-expect (any-destory? (make-posn 10 20) (cons (make-posn 10 100) '())) #false)
(check-expect (any-destory? (make-posn 10 20) (cons (make-posn 10 20) '())) #true)
(define (any-destory? u ms)
  (cond
    [(empty? ms) #false]
    [else (if (destory? u (first ms)) #true (any-destory? u (rest ms)))]))

; SIGS -> Boolean
; check if the game finished or not
(check-expect (si-game-over? (make-aim (make-posn 10 UFO-MAX-Y) (make-tank 10 3))) #true)
(check-expect (si-game-over? (make-aim (make-posn 10 20) (make-tank 10 3))) #false)
(check-expect (si-game-over? (make-fired (make-posn 10 UFO-MAX-Y) (make-tank 10 3) (cons (make-posn 10 20) '()))) #true)
(check-expect (si-game-over? (make-fired (make-posn 10 20) (make-tank 10 3) (cons (make-posn 10 20) '()))) #true)
(check-expect (si-game-over? (make-fired (make-posn 10 20) (make-tank 10 3) (cons (make-posn 10 100) '()))) #false)
(define (si-game-over? s)
  (cond
    [(aim? s) (ufo-landed? (aim-ufo s))]
    [(fired? s) (or (ufo-landed? (fired-ufo s)) (any-destory? (fired-ufo s) (fired-missiles s)))]))

; SIGS -> Image
; render the final status
(define (si-render-final s)
  (cond
    [(aim? s) (si-render s)]
    [(fired? s) (if (any-destory? (fired-ufo s) (fired-missiles s))
                    (tank-render (fired-tank s) BACKGROUND) (si-render s))]))

; Missile -> Missile
; Move missile for one tick
(check-expect (move-missile (make-posn 10 20)) (make-posn 10 (- 20 MISSILE-SPEED)))
(define (move-missile m)
  (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED)))

; ListOfMissile -> ListOfMissile
; Move a list of missiles
(check-expect (move-missiles (cons (make-posn 10 20) '())) (cons (make-posn 10 (- 20 MISSILE-SPEED)) '()))
(define (move-missiles ms)
  (cond
    [(empty? ms) ms]
    [else (cons (move-missile (first ms)) (move-missiles (rest ms)))]))


; UFO -> UFO
; move UFO for one tick
(check-expect (move-ufo (make-posn 10 20) 3) (make-posn 13 (+ 20 UFO-SPEED)))
(check-expect (move-ufo (make-posn 5 20) -10) (make-posn 15 (+ 20 UFO-SPEED)))
(define (move-ufo u delta)
  (make-posn (if (< 0 (+ (posn-x u) delta) WIDTH)
                 (+ (posn-x u) delta)
                 (- (posn-x u) delta))
             (+ UFO-SPEED (posn-y u))))

; Missile -> Boolean
; check if a missile is in the scene or not
(check-expect (check-missile (make-posn 10 20)) #true)
(check-expect (check-missile (make-posn 10 -20)) #false)
(define (check-missile m)
  (> (posn-y m) 0))

; ListOfMissiles -> Boolean
; check if there are any missile inside the scence
(check-expect (any-missile? (cons (make-posn 10 20) '())) #true)
(check-expect (any-missile? (cons (make-posn 10 -20) '())) #false)
(define (any-missile? ms)
  (cond
    [(empty? ms) #false]
    [else (if (check-missile (first ms)) #true (any-missile? (rest ms)))]))

; ListOfMissiles -> ListOfMissiles
; filter the missiles outside of the scene
(check-expect (filter-missiles (cons (make-posn 10 -20) '())) '())
(check-expect (filter-missiles (cons (make-posn 10 20) '())) (cons (make-posn 10 20) '()))
(define (filter-missiles ms)
  (cond
    [(empty? ms) ms]
    [else (if (check-missile (first ms))
              (cons (first ms) (filter-missiles (rest ms)))
              (filter-missiles (rest ms)))]))

; SIGS Number -> SIGS
; move the space-invader objects predictably by delta
(check-expect (si-move-proper (make-aim (make-posn 10 20) (make-tank 10 3)) 10)
              (make-aim (make-posn 20 (+ 20 UFO-SPEED)) (make-tank 10 3)))
(check-expect (si-move-proper (make-fired (make-posn 10 20) (make-tank 10 3) (cons (make-posn 10 30) '())) 10)
              (make-fired (make-posn 20 (+ UFO-SPEED 20)) (make-tank 10 3) (cons (make-posn 10 (- 30 MISSILE-SPEED)) '())))
(define (si-move-proper w delta)
  (cond
    [(aim? w) (make-aim (move-ufo (aim-ufo w) delta)
                        (aim-tank w))]
    [(fired? w)
     (cond
       [(any-missile? (fired-missiles w))
        (make-fired (move-ufo (fired-ufo w) delta)
                    (fired-tank w)
                    (filter-missiles (move-missiles (fired-missiles w))))]
       [else
        (make-aim
         (move-ufo (fired-ufo w) delta)
         (fired-tank w))])]))

(define RANDOM-JUMP 10)
; SIGS -> SIGS
; UFO random move left or right
(define (si-move w)
  (si-move-proper w (- (random RANDOM-JUMP) (/ RANDOM-JUMP 2))))

; Tank KeyEvent -> Tank
; move a Tank based on the input
(check-expect (move-tank (make-tank 10 3) "left") (make-tank 7 3))
(check-expect (move-tank (make-tank 10 3) "right") (make-tank 13 3))
(define (move-tank t k)
  (cond
    [(string=? k "left") (make-tank (- (tank-loc t) (tank-vel t)) (tank-vel t))]
    [(string=? k "right") (make-tank (+ (tank-loc t) (tank-vel t)) (tank-vel t))]))

; SIGS KeyEvent -> SIGS
; control the tank based on key input
(define (si-control w k)
  (cond
    [(or (string=? k "left") (string=? k "right"))
     (cond
       [(aim? w) (make-aim (aim-ufo w)
                           (move-tank (aim-tank w) k))]
       [(fired? w) (make-fired (fired-ufo w)
                               (move-tank (fired-tank w) k)
                               (fired-missiles w))])]
    [(string=? k " ")
     (cond
       [(aim? w) (make-fired (aim-ufo w) (aim-tank w) (cons (make-posn (tank-loc (aim-tank w)) TANK-Y) '()))]
       [(fired? w) (make-fired (fired-ufo w)
                               (fired-tank w)
                               (cons (make-posn (tank-loc (fired-tank w)) TANK-Y) (fired-missiles w)))])]
    [else w]))

(define (main w)
  (big-bang w
            [to-draw si-render]
            [on-key si-control]
            [on-tick si-move]
            [stop-when si-game-over? si-render-final]))

; Exercise160
