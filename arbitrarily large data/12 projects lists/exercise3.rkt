;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise 215
(define WIDTH 200)
(define HEIGHT 300)
(define RADIUS 5)
(define MAX-WIDTH (/ WIDTH (* 2 RADIUS)))
(define MAX-HEIGHT (/ HEIGHT (* 2 RADIUS)))
(define VEL (* RADIUS 2))
(define MPTS (empty-scene WIDTH HEIGHT))

; vel is a struct
; (make-vel x y)
; x is the vel of x axis, y is the vel of y axis
(define-struct vel [x y])
(make-vel 1 0)
(make-vel 0 1)

; snake is a struct
; (make-snake vel posns)
; vel is the velocity of the snake
; posns is a list of posns, each is the posn a dot of the snake's body
(define-struct snake [vel posns])
(define s1 (make-snake (make-vel 1 0) (cons (make-posn 10 20) '())))

; wc is a struct
; (make-wc food snake)
; food is a posn
; snake is a snake
(define-struct wc [food snake])
(define w1 (make-wc (make-posn 20 30) s1))


; vel k -> boolean
; the key is valid or not
(check-expect (invalid-key (make-vel 1 0) "left") #true)
(define (invalid-key v k)
  (or
   (and (> (vel-x v) 0) (string=? "left" k))
   (and (< (vel-x v) 0) (string=? "right" k))
   (and (> (vel-y v) 0) (string=? "up" k))
   (and (< (vel-y v) 0) (string=? "down" k))))

; wc key -> wc
; move the snake based on the key pressed
(check-expect (key-handler w1 "up") (make-wc (wc-food w1)
                                             (make-snake (make-vel 0 (* -1 VEL)) (snake-posns (wc-snake w1)))))
(define (key-handler wc k)
  (if (invalid-key (snake-vel (wc-snake wc)) k) wc
      (make-wc (wc-food wc)
               (cond
                 [(string=? k "left") (make-snake (make-vel (* -1 VEL) 0) (snake-posns (wc-snake wc)))]
                 [(string=? k "right") (make-snake (make-vel VEL 0) (snake-posns (wc-snake wc)))]
                 [(string=? k "up") (make-snake (make-vel 0 (* -1 VEL)) (snake-posns (wc-snake wc)))]
                 [(string=? k "down") (make-snake (make-vel 0 VEL) (snake-posns (wc-snake wc)))]
                 [else (wc-snake wc)]))))

; vel posn -> posn
; move the head of the snake based on vel
(check-expect (move-head (make-vel 1 0) (make-posn 20 10)) (make-posn 21 10))
(define (move-head v p)
  (make-posn (modulo (+ (posn-x p) (vel-x v)) WIDTH)
             (modulo (+ (posn-y p) (vel-y v)) HEIGHT)))

; list-of-posns -> list-of-posns
; remove the last element of the list
(check-expect (remove-last (list 1 2 3 4)) (list 1 2 3))
(define (remove-last lop)
  (cond
    [(empty? lop) '()]
    [(empty? (rest lop)) '()]
    [else (cons (first lop) (remove-last (rest lop)))]))

; snake -> snake
; snake move on tick
(check-expect (tock s1) (make-snake (make-vel 1 0) (cons (make-posn 11 20) '())))
(define (tock s)
  (make-snake (snake-vel s)
              (cons (move-head (snake-vel s) (first (snake-posns s))) (remove-last (snake-posns s)))))

; wc -> wc
; eat the food and create a new food
(define (eat_and_create wc)
  (make-wc
   (make-posn (+ RADIUS (* (* RADIUS 2) (random MAX-WIDTH)))
              (+ RADIUS (* (* RADIUS 2) (random MAX-HEIGHT))))
   (make-snake (snake-vel (wc-snake wc))
               (cons (wc-food wc) (snake-posns (wc-snake wc))))))

; posn posn -> boolean
; check the two posn equal or not
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

; wc -> wc
; wc change on tick
(define (tock.v2 wc)
  (cond
    [(posn=? (wc-food wc) (first (snake-posns (tock (wc-snake wc))))) (eat_and_create wc)]
    [else (make-wc (wc-food wc) (tock (wc-snake wc)))]))


; list-of-posns image -> image
; add list-of-posns to the image
(define (render-posns lop img)
  (cond
    [(empty? lop) img]
    [else (place-image (circle RADIUS "solid" "red")
                       (posn-x (first lop)) (posn-y (first lop))
                       (render-posns (rest lop) img))]))
; snake -> image
; render the snake
(define (render s)
  (render-posns (snake-posns s) MPTS))

; wc -> image
; render the wc
(define (render.v2 wc)
  (place-image (circle RADIUS "solid" "green")
               (posn-x (wc-food wc)) (posn-y (wc-food wc))
               (render (wc-snake wc))))

; snake -> boolean
; game end or not
(define (end? s)
  (member? (first (snake-posns s)) (rest (snake-posns s))))

; wc -> boolean
; game end or not
(define (end?.v2 wc)
  (end? (wc-snake wc)))

(define (worm-main s)
  (big-bang s
            [on-key key-handler]
            [on-tick tock.v2]
            [to-draw render.v2]
            [stop-when end?.v2]))