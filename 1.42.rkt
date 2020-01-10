#lang sicp

(define (square x) (* x x))

(define (compose2 f g)
  (lambda (x) (f (g x))))


(newline)
(display "
((compose2 square inc) 6) = ")
((compose2 square inc) 6)




(define (compose . fs)
  (if (null? fs)
      identity
      (lambda (x) ((car fs) ((apply compose (cdr fs)) x)))))


(display "
((compose square inc inc) 6) = ")
((compose square inc inc) 6)

(newline)
