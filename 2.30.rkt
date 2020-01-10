#lang sicp
(newline)

(define (square x) (* x x))




(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))


(define (sq-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (sq-tree subtree)
             (square subtree)))
       tree))
                       



(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(display "tree = ")
tree


(display "Implemented without map, (square-tree tree) = ")
(square-tree tree)

(display "Implemented with map,    (square-tree tree) = ")
(sq-tree tree)




(newline)
