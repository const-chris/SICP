#lang sicp
(newline)

(define (sum . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply sum (cdr xs)))))


(define (average . xs)
  (/ (apply sum xs) (length xs)))


(define (compose . fs)
  (if (null? fs)
      identity
      (lambda (x) ((car fs) ((apply compose (cdr fs)) x)))))


(define (repeated f n)
  (define (iter result count)
    (if (<= count 0)
        result
        (iter (compose f result) (- count 1)))) 
  (iter identity n))


(define (smooth dx)
  (lambda (f)
    (lambda (x) (average (f x)
                         (f (- x dx))
                         (f (+ x dx))))))




(define dx 0.0001)

(display (string-append "(((smooth " (number->string dx) ") sin) 0) = "))
(((smooth dx) sin) 0)

(display (string-append "(((smooth " (number->string dx) ") cos) 0) = "))
(((smooth dx) cos) 0)

(display (string-append "(((repeated (smooth " (number->string dx) ") cos) 0) = "))
(((repeated (smooth dx) 10) cos) 0)




(newline)