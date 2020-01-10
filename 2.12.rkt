#lang sicp
(newline)

(define (average . xs)
  (/ (apply + xs) (length xs)))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))




(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (center x)
  (average (lower-bound x) (upper-bound x)))

(define (percent x)
  (let ((c (center x)))
    (/ (- (upper-bound x) c) c)))




(define test-interval (make-center-percent 50 0.02))
(display "test-interval = (make-center-percent 50 0.02) = ")
test-interval

(display "(center test-interval) = ")
(center test-interval)

(display "(percent test-interval) = ")
(percent test-interval)




(newline)