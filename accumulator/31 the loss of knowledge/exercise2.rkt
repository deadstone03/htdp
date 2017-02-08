;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; Exercise 509
(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; render a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))
 
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of 
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right

; List-Of-Item List-Of-Item -> Boolean
; two lists are equal or not
(check-expect (list=? (list 1 2 3) (list 1 2 3)) #true)
(check-expect (list=? (list 1 2) (list 1 2 3)) #false)
(define (list=? la lb)
  (cond
    [(empty? la) (empty? lb)]
    [else (and (equal? (first la) (first lb)) (list=? (rest la) (rest lb)))]))

(define (f los x)
  (lambda (e)
    (and
     (list=? los (append (editor-pre e) (editor-post e)))
     (<= (image-width (editor-text (editor-pre e)))
         x
         (image-width (editor-text (append (editor-pre e) (list (first (editor-post e))))))))))
; List-Of-1Strings Number -> Edit
; split the list of string, so that it will create a edit (pre, post)
; pre + post = los
; pre's image's width less than x, and pre + 1 1String's width larger than x
(check-satisfied (split-structural (list "a" "b" "c" "d") 23) (f (list "a" "b" "c" "d") 23))
(define (split-structural los x)
  (local
    (
     ; list-of-1string number list-of-1string
     ; split the string
     ; accumulator the string before los
     (define (split-structural/a los x a)
       (cond
         [(empty? los) (make-editor a '())]
         [(<= (image-width (editor-text a)) x (image-width (editor-text (append a (list (first los))))))
          (make-editor a los)]
         [else (split-structural/a (rest los) x (append a (list (first los))))])))
    (split-structural/a los x '())))