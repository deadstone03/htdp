;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
'()
(cons "Mercury" '())

; Exercise129
; 1
(cons "p1" (cons "p2" (cons "p3" (cons "p4" (cons "p5" (cons "p6" (cons "p7" (cons "p8" (cons "p9" '())))))))))
; 2
(cons "steak" (cons "french fries" (cons "beans" (cons "bread" (cons "water" (cons "brie cheese" (cons "ice creem" '())))))))
; 3
(cons "yellow" (cons "blue" (cons "green" '())))

(define (generate count)
  (if (= count 0) (cons 0 '()) (cons count (generate (- count 1)))))

(define (generate1 b e)
  (if (= b e) (cons b '()) (cons b (generate1 (+ b 1) e))))

; Exercise130
(cons "s1" (cons "s2" (cons "s3" (cons "s4" (cons "s5" '())))))

; Exercise131
; ListBoolean is one of these
; '()
; (cons Boolean ListBoolean)

; List-of-names -> Boolean
; determines whether "Flatt" is on a-list-of-names
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect (contains-flatt? (cons "A" (cons "Flatt" (cons "c" '())))) #true)
(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(string=? (first alon) "Flatt") #true]
    [else (contains-flatt? (rest alon))]))

(check-expect (contains-flatt? (cons "Fagan"
                                     (cons "Findler"
                                           (cons "Fisler"
                                                 (cons "Flanagan"
                                                       (cons "Flatt"
                                                             (cons "Felleisen"
                                                                   (cons "Friedman" '())))))))) #true)

; Exercise133
; Exercise134
; String ListOfString -> Boolean
; the list of string has the string
(check-expect (my-member? "a" '()) #false)
(check-expect (my-member? "a" (cons "a" '())) #true)
(check-expect (my-member? "a" (cons "b" '())) #false)
(check-expect (my-member? "a" (cons "b" (cons "c" (cons "a" '())))) #true)
(define (my-member? s alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon) (or (string=? s (first alon)) (my-member? s (rest alon)))]
    [else #false]))