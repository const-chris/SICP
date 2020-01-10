#lang sicp
(newline)

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (mult-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))




(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
      (error "division by interval spanning zero:" y)
      (mult-interval x
                     (make-interval (/ 1 (upper-bound y))
                                    (/ 1 (lower-bound y))))))




(define interval-one (make-interval -2 4))
(define interval-two (make-interval -3 -1))
(define interval-three (make-interval -3 1))


(display interval-one)
(display " / ")
(display interval-two)
(display " = ")
(div-interval interval-one interval-two)


(display interval-one)
(display " / ")
(display interval-three)
(display " = ")
(div-interval interval-one interval-three)




(newline)
