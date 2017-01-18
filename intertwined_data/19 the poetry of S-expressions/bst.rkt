;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BinaryTree (short for BT) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define ABT (make-node
             15
             'd
             NONE
             (make-node
              24 'i NONE NONE)))
     
; Exercise322
; BT Number -> Boolean
; check if any node's ssn is n
(check-expect (contains-bt? ABT 24) #true)
(check-expect (contains-bt? ABT 10) #false)
(define (contains-bt? bt n)
  (cond
    [(no-info? bt) #false]
    [else (if (equal? (node-ssn bt) n)
              #true
              (or (contains-bt? (node-left bt) n)
                  (contains-bt? (node-right bt) n)))]))
; Exercise323
; Exercise324
; BT -> list-of-number
; inorder of the tree
(check-expect (inorder ABT) (list 15 24))
(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else (append (inorder (node-left bt)) (list (node-ssn bt)) (inorder (node-right bt)))]))

(define ABST (make-node
             15
             'd
             (make-node
              10 'b NONE NONE)
             (make-node
              24 'i NONE NONE)))
; Exercise325
; BST n -> Name or NONE
(check-expect (bst-search ABST 10) 'b)
(check-expect (bst-search ABST 5) NONE)
(define (bst-search bst n)
  (cond
    [(no-info? bst) NONE]
    [(equal? (node-ssn bst) n) (node-name bst)]
    [(< (node-ssn bst) n) (bst-search (node-right bst) n)]
    [else (bst-search (node-left bst)n) ]))


; BST n s -> BST
; insert one new node in the BST
(check-expect (create-bst ABST 13 'c)
              (make-node
               15
               'd
               (make-node
                10 'b NONE
                (make-node
                 13 'c NONE NONE))
               (make-node
                24 'i NONE NONE)))
(define (create-bst bst n s)
  (cond
    [(no-info? bst) (make-node n s NONE NONE)]
    [(< n (node-ssn bst)) (make-node (node-ssn bst) (node-name bst) (create-bst (node-left bst) n s) (node-right bst))]
    [else (make-node (node-ssn bst) (node-name bst) (node-left bst) (create-bst (node-right bst) n s))]))

; [List-of [List Number Symbol]] -> BST
; Create a bst from the list
(define (create-bst-from-list l)
  (local
    (
     ; BST [Lis Number Symbol] -> BST
     ; insert one node
     (define (create-bst-l l bst)
       (create-bst bst (first l) (second l))))
    (foldr create-bst-l NONE l)))