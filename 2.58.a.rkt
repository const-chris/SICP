#lang racket
(require "symbolic-differentiation.rkt")
(newline)


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))                          
        (else (error "unknown expression type -- DERIV" exp))))




(define (sum? exp) (and (pair? exp) (eq? (cadr exp) '+)))
(define (addend exp) (car exp))
(define (augend exp) (caddr exp))
(define (product? exp) (and (pair? exp) (eq? (cadr exp) '*)))
(define (multiplier exp) (car exp))
(define (multiplicand exp) (caddr exp))

(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y) (+ x y)))
        (else (list x '+ y))))

(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list x '* y))))

(display "(deriv '(x + (3 * (x + (y + 2)))) 'x) = ")
(deriv '(x + (3 * (x + (y + 2)))) 'x)




(newline)