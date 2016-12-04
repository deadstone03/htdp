; Exercise59
; Exercise60
; Exercise61
; Exercise62
; Exercise63
; Exercise64
(check-expect (manhattan-distance (make-posn 10 20))30)
(define (manhattan-distance ap)
  (+ (posn-x ap) (posn-y ap)))

; Exercise65
; Exercise66
; Exercise67
(define-struct ball [location velocity])
(define SPEED 3)
(define-struct balld [location direction])
(make-balld 10 "up")
(make-balld 20 "down")
; Exercise68
; Exercise69
; Exercise70
; Exercise71
; distances in terms of pixels:
(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH  400)
(define CENTER (quotient WIDTH 2))
 
(define-struct game [left-player right-player ball])
 
(define game0
  (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))
(game-ball game0)
(posn? (game-ball game0))
(game-left-player game0)

; Exercise72
; a phone is a structure:
;   (make-phone String String)
; interpretation (make-phone area number) means a phone number
; area is the area number
; number is the phone number
(define-struct phone (area number))
