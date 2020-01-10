#lang racket
(newline)
(provide make-tree entry left-branch right-branch tree->list)

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))


(define tree1 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 5 '() '()))
                         (make-tree 9
                                    '()
                                    (make-tree 11 '() '()))))

(define tree2 (make-tree 3
                         (make-tree 1 '() '())          
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9
                                               '()
                                               (make-tree 11 '() '())))))

(define tree3 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))




(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
                            (cons (entry tree)
                                  (tree->list-1 (right-branch tree))))))

(display "tree->list-1:\n")
(display tree1)
(display " -> ")
(tree->list-1 tree1)
(display tree2)
(display " -> ")
(tree->list-1 tree2)
(display tree3)
(display " -> ")
(tree->list-1 tree3)
(newline)




(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(display "tree->list-2:\n")
(display tree1)
(display " -> ")
(tree->list-2 tree1)
(display tree2)
(display " -> ")
(tree->list-2 tree2)
(display tree3)
(display " -> ")
(tree->list-2 tree3)




; The two procedures will produce the same list from every binary tree.
; Both procedures make the same number of recursive calls to convert a
; tree of n elements to a list (2n + 1).
; However, tree->list-1 makes a call to append--an O(n) operation--on each
; recursive call, while tree->list-2 calls only itself and cons--an O(1)
; operation.
; Therefore, the order of growth in the number of steps to convert a
; binary tree of n elements to a list for tree->list-1 is O(n^2),
; and for tree->list-2 it is O(n).



(define tree->list tree->list-2)
(newline)