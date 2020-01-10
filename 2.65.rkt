#lang racket
(require "2.64.rkt")

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define tree1 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 5 '() '()))
                         (make-tree 9
                                    '()
                                    (make-tree 11 '() '()))))

(define tree2 (make-tree 4
                         (make-tree 1 '() '())          
                         (make-tree 8
                                    (make-tree 5 '() '())
                                    (make-tree 9
                                               '()
                                               (make-tree 12 '() '())))))

(display "tree1 = ")
tree1
(display "tree2 = ")
tree2
(newline)




(define (union-set set1 set2)
  (define (union-ordered-lists setX setY)
    (cond ((null? setX) setY)
          ((null? setY) setX)
          (else
           (let ((x (car setX))
                 (xs (cdr setX))
                 (y (car setY))
                 (ys (cdr setY)))
             (cond ((= x y) (cons x (union-ordered-lists xs ys)))
                   ((< x y) (cons x (union-ordered-lists xs setY)))
                   ((> x y) (cons y (union-ordered-lists setX ys))))))))          
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (union-ordered-lists list1 list2))))
   

(display "(union-set tree1 tree2) = ")
(union-set tree1 tree2)




(define (intersection-set set1 set2)
  (define (intersection-ordered-lists setX setY)
    (if (or (null? setX) (null? setY))
        '()
        (let ((x (car setX))
              (xs (cdr setX))
              (y (car setY))
              (ys (cdr setY)))
          (cond ((= x y) (cons x (intersection-ordered-lists xs ys)))
                ((< x y) (intersection-ordered-lists xs setY))
                ((> x y) (intersection-ordered-lists setX ys))))))
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (intersection-ordered-lists list1 list2))))

(display "(intersection-set tree1 tree2) = ")
(intersection-set tree1 tree2)




(newline)