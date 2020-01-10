#lang sicp

(define (square x) (* x x))


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




(display "
((repeated square 2) 5) = ")
((repeated square 2) 5)

(newline)


