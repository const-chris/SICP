#lang racket
(newline)



(define (union-set setX setY)
  (cond ((null? setX) setY)
        ((null? setX) setY)
        (else
         (let ((x (car setX))
               (xs (cdr setX))
               (y (car setY))
               (ys (cdr setY)))
           (cond ((= x y) (cons x (union-set xs ys)))
                 ((< x y) (cons x (union-set xs setY)))
                 ((> x y) (cons y (union-set setX ys))))))))


(display "(union-set '(1 2 4 5) '(1 3 4 6)) = ")
(union-set '(1 2 4 5) '(1 3 4 6))




(newline)