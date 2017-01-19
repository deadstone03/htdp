;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |refining data definitions|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

; Exercise330

(define DT.v1 (list (list (list "part1" "part2" "part3") "read!" (list (list "hang" "draw") (list "read!")))))

; Exercise331
; Dir.v1 -> Number
; how many file the dir contains
(check-expect (how-many DT.v1) 7)
(define (how-many dt)
  (cond
    [(string? dt) 1]
    [else (foldr (lambda (dt s) (+ (how-many dt) s)) 0 dt)]))

(define-struct dir [name content])
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; A LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

; Exercise332
(define DT.v2 (make-dir "TS"
                        (list
                         (make-dir "Test" (list "part1" "part2" "part3"))
                         "read!"
                         (make-dir "Libs" (list
                                           (make-dir "Code" (list "hang" "draw"))
                                           (make-dir "Docs" (list "read!")))))))

; Exercise333
(check-expect (how-many.v2 DT.v2) 7)
(define (how-many.v2 dt)
  (cond
    [(dir? dt) (foldr (lambda (dt s) (+ (how-many.v2 dt) s)) 0 (dir-content dt))]
    [else 1]))


(define-struct file [name size content])
(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

; Exercise335
(define DT.v3 (make-dir.v3 "TS"
                           (list
                            (make-dir.v3 "Test"
                                         '()
                                         (list (make-file "part1" 99 "")
                                               (make-file "part2" 52 "")
                                               (make-file "part3" 17 "")))
                            (make-dir.v3 "Libs"
                                         (list
                                          (make-dir.v3 "Code"
                                                       '()
                                                       (list (make-file "hang" 8 "")
                                                             (make-file "draw" 2 "")))
                                          (make-dir.v3 "Docs"
                                                       '()
                                                       (list (make-file "read!" 19 ""))))
                                         '()))
                           (list (make-file "read!" 10 ""))))

; Exercise336
(check-expect (how-many.v3 DT.v3) 7)
(define (how-many.v3 dt)
  (cond
    [(dir.v3? dt) (+ (length (dir.v3-files dt))
                     (foldr (lambda (dt s) (+ (how-many.v3 dt) s)) 0 (dir.v3-dirs dt)))]
    [else 1]))

; Exercise337
