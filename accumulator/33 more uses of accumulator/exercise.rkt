;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; Exercise 528
(define WIDTH 1000)
(define HEIGHT 1000)
(define MTS (empty-scene WIDTH HEIGHT))
; Image Number Number Length Angle -> Image
; plot a line on the scene
(define (draw-line s x y l a)
  (local
    (
     (define r-angle (* pi (/ a 180))))
    (scene+line s x y (+ x (* (cos r-angle) l)) (- y (* (sin r-angle) l)) "red")))


(define left-ratio (/ 2 3))
(define right-ratio 0.8)
(define left-delta-a (* (/ -0.15 pi) 180))
(define right-delta-a (* (/ 0.2 pi) 180))
; Number -> Image
; draw a tree
(define (draw-lines init-l)
  (local
    (
     (define (draw-lines/a s x y l a)
       (cond
         [(< l 10) s]
         [else
          (local
            (
             (define s1 (draw-line s x y l a))
             (define r-angle (* pi (/ a 180)))
             (define x1 (+ x (* (cos r-angle) l)))
             (define y1 (- y (* (sin r-angle) l)))
             (define s2 (draw-lines/a s1
                                      (/ (+ x1 (* 2 x)) 3)
                                      (/ (+ y1 (* 2 y)) 3)
                                      (* l left-ratio)
                                      (+ a left-delta-a)))
             (define s3 (draw-lines/a s2
                                      (/ (+ x (* 2 x1)) 3)
                                      (/ (+ y (* 2 y1)) 3)
                                      (* l right-ratio)
                                      (+ a right-delta-a))))
            s3)])))
  (draw-lines/a MTS (/ WIDTH 2) HEIGHT init-l 90)))