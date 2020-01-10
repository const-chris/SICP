#lang sicp
(newline)

(define (square x) (* x x))


(define (sum . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply sum (cdr xs)))))


(define (average . xs)
  (/ (apply sum xs) (length xs)))


(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improve guess))))
    (iter first-guess)))




(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess))))
   1.0))


(display "√49 ≈ ")
(sqrt 49)

(display "√16 ≈ ")
(sqrt 16)

(display "√225 ≈ ")
(sqrt 225)

(newline)




(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) 0.000001))
                      (lambda (guess) (f guess)))
   first-guess))


(display "x = cos(x)
x ≈ ")
(fixed-point cos 1.0)

(display "y = sin(y) + cos(y)
y ≈ ")
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)



(newline)