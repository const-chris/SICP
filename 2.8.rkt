#lang sicp
(newline)

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))




(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))




(define interval-one (make-interval -2 4))
(define interval-two (make-interval -1 10))


(display interval-one)
(display " - ")
(display interval-two)
(display " = ")
(sub-interval interval-one interval-two)




(newline)