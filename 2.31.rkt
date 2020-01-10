#lang sicp
(newline)

(define (square x) (* x x))




(define (tree-map proc tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map proc subtree)
             (proc subtree)))
       tree))




(define (square-tree tree)
  (tree-map square tree))




(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(display "tree = ")
tree


(display "Implemented with tree-map, (square-tree tree) = ")
(square-tree tree)




(newline)