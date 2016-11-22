;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fixed_size_data5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Exercise33

; Exercise34
; extract the first character of a non-empty sring s
; string -> 1string
; given "abc"; expected "a"
; given "1234"; expected "1"
(define (string-first s)
  (string-ith s 0))

; Exercise35
; extract the last character of a non-empty string s
; string-> 1string
; given "abc"; expected "c"
(define (string-last s)
  (string-ith s (- (string-length s) 1)))

; Exercise36
; caculate the area of an image img
; image -> number
; given (circle 20 "solid" "red"); expected 1600
(define (image-area img)
  (* (image-width img) (image-height img)))

; Exercise37
; remove the first character for the string s
; string -> string
; given "abc"; expected "bc"
(define (string-rest s)
  (substring s 1 (string-length s)))

; Exercise38
; remove the last character of the string s
; string -> string
; given "abc"; expected "ab"
(define (string-remove-last s)
  (substring s 0 (- (string-length s) 1)))

