;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname database) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define-struct db [schema content])
; A DB is a structure:
;   (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

; Exercise 404
; [x y] [x y -> boolean] [list-of x] [list-of y] -> boolean
; check every pair is #true or not
(check-expect (andmap2 equal? (list 1 2 3 4) (list 1 2 3 4)) #true)
(check-expect (andmap2 equal? (list 1 2 3 4) (list 1 2 3 5)) #false)
(define (andmap2 f l1 l2)
  (cond
    [(not (= (length l1) (length l2))) #false]
    [(and (empty? l1) (empty? l2)) #true]
    [else (and (f (first l1) (first l2)) (andmap2 f (rest l1) (rest l2)))]))

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))
 
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
 
(define school-db
  (make-db school-schema
           school-content))
 

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
 
(define projected-db
  (make-db projected-schema projected-content))
 
(check-expect (db-content (project.v1 school-db '("Name" "Present")))
              projected-content)
(define (project.v1 db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (local
              ((define schema-labels (map first schema)))
            (foldr (lambda (c n res)
                     (if (member? n labels) (cons c res) res)) '() row schema-labels))))
    (make-db (filter keep? schema)
             (map row-project content))))

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row
          ; retain those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (make-db (filter keep? schema)
             (map row-project content))))

; Exercise 408
(define (make-predicate schema label f)
  ; row -> Boolean
  ; check the row's label's value satisfied the f
  (lambda (row)
    (local
      (
       (define (get-value row schema)
         (cond
           [(equal? label (first (first schema))) (first row)]
           [else (get-value (rest row) (rest schema))])))
      (f (get-value row schema)))))

(define selected-content
  `(("Alice" 35)
    ("Carol" 30)
    ("Dave"  32)))
; db list-of-labels [row -> boolean] -> db
; select the rows that satisify the predicte
(check-expect (db-content (select school-db '("Name" "Age") (make-predicate school-schema "Age" (lambda (x) (>= x 30)))))
              selected-content)
(define (select db labels predicate)
  (project (make-db (db-schema db) (filter predicate (db-content db))) labels))

; Exercise 409
(define reordered-content
    `(("Alice" #true 35)
      ("Bob" #false 25)
      ("Carol" #true 30)
      ("Dave" #false 32)))
(check-expect (db-content (reorder school-db '("Name" "Present" "Age")))
              reordered-content)
(define (reorder db labels)
  (local
    (
     (define schema (db-schema db))
     (define content (db-content db))
     (define (get-schema schema label)
       (if (equal? (first (first schema)) label) (first schema) (get-schema (rest schema) label)))
     (define (reorder-schema schema labels)
       (cond
         [(empty? labels) '()]
         [else (cons (get-schema schema (first labels)) (reorder-schema schema (rest labels)))]))
     (define (get-content row schema label)
       (if (equal? (first (first schema)) label) (first row) (get-content (rest row) (rest schema) label)))
     (define (reorder-row row schema labels)
       (cond
         [(empty? labels) '()]
         [else (cons (get-content row schema (first labels)) (reorder-row row schema (rest labels)))]))
     (define (reorder-content content schema labels)
       (map (lambda (row) (reorder-row row schema labels)) content))
     )
    (make-db
     (reorder-schema schema labels)
     (reorder-content content schema labels))))

; Exercise 410

(define school-content2
  `(("Alice2" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave2"  32 #false)))
(define unioned-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)
    ("Alice2" 35 #true)
    ("Dave2"  32 #false)))
(define school-db2 (make-db school-schema school-content2))
; db db -> db
; union two db
(check-expect (db-content (db-union school-db school-db2))
              unioned-content)
(define (db-union db1 db2)
  (local
    (
     (define (row=? r1 r2)
       (cond
         [(and (empty? r1) (empty? r2)) #true]
         [else (and (equal? (first r1) (first r2)) (row=? (rest r1) (rest r2)))]))
     (define (content-insert row c1)
       (cond
         [(empty? c1) (cons row '())]
         [else (if (row=? (first c1) row) c1 (cons (first c1) (content-insert row (rest c1))))]))
     (define (content-union c1 c2)
       (foldl content-insert c1 c2)))
  (make-db (db-schema db1)
             (content-union (db-content db1) (db-content db2)))))


	
(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))
(define presence-content
  '((#true  "presence1")
    (#true  "presence2")
    (#false "absence")))
(define presence-db
  (make-db presence-schema
           presence-content))

(define joined-content
  (list
   (list "Dave" 32 "absence")
   (list "Bob" 25 "absence")
   (list "Carol" 30 "presence2")
   (list "Alice" 35 "presence2")
   (list "Carol" 30 "presence1")
   (list "Alice" 35 "presence1")))
(check-expect (db-content (join school-db presence-db))
              joined-content)
(define (join db1 db2)
  (local
    (
     (define schema1 (db-schema db1))
     (define schema2 (db-schema db2))
     (define content1 (db-content db1))
     (define content2 (db-content db2))
     (define (join-schema s1 s2)
       (cond
         [(empty? (rest s1)) (rest s2)]
         [else (cons (first s1) (join-schema (rest s1) s2))]))
     (define (row=? r1 r2)
       (cond
         [(and (empty? r1) (empty? r2)) #true]
         [else (and (equal? (first r1) (first r2)) (row=? (rest r1) (rest r2)))]))
     (define (content-insert row c1)
       (cond
         [(empty? c1) (cons row '())]
         [else (if (row=? (first c1) row) c1 (cons (first c1) (content-insert row (rest c1))))]))
     (define (content-union c1 c2)
       (foldl content-insert c1 c2))
     (define (join? r1 r2)
       (cond
         [(empty? (rest r1)) (equal? (first r1) (first r2))]
         [else (join? (rest r1) r2)]))
     (define (join-row r c)
       (foldl (lambda (c_r res) (if (join? c_r r) (cons (join-schema c_r r) res) res)) '() c))
     (define (join-content c1 c2)
       (foldl (lambda (r joined-c) (content-union (join-row r c1) joined-c)) '() c2))
     )
    (make-db
     (join-schema schema1 schema2)
     (join-content content1 content2))))