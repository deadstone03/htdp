;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |refining functions|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define O (create-dir "/Users/xingdai/learn/python-docs-samples"))

; Exercise 338
; dt -> number
; get the number of files
(define (how-many dt)
  (cond
    [(dir? dt) (+ (length (dir-files dt))
                  (foldr (lambda (dt s) (+ s (how-many dt))) 0 (dir-dirs dt)))]
    [else 1]))

; Exercise 339
; dt name -> boolean
(define (find? dt name)
  (cond
    [(dir? dt) (or (ormap (lambda (dt) (find? dt name)) (dir-files dt))
                   (ormap (lambda (dt) (find? dt name)) (dir-dirs dt)))]
    [else (if (string=? name (file-name dt)) #true #false)]))

; Exercise 340
; dt -> list-of-strings
; show the file and dir's names under the dir
(define (ls dt)
  (append (map file-name (dir-files dt)) (map dir-name (dir-dirs dt))))