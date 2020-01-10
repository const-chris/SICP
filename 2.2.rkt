#lang sicp

(define (average . xs)
  (/ (apply + xs)
     (length xs)))




(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))




(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))




(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define (midpoint-test x1 y1 x2 y2)  
  (let ((a (make-point x1 y1))
        (b (make-point x2 y2)))
    (let ((s (make-segment a b)))
      (newline)
      (display "midpoint of segment from ")
      (print-point a)
      (display " to ")
      (print-point b)
      (display ": ")
      (print-point (midpoint-segment s))
      (newline))))


(midpoint-test 0 0 0 1)
(midpoint-test 0 0 0 -1)
(midpoint-test 0 0 1 1)
(midpoint-test 1 0 0 1)
(midpoint-test 0 1 0 0)
(midpoint-test -1 -1 1 1)




(newline)