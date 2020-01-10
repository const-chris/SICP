#lang racket
(require "symbolic-differentiation.rkt")
(provide make-exponentiation)
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
        ((exponentiation? exp) (make-product (make-product (exponent exp)
                                                           (make-exponentiation (base exp)
                                                                                (- (exponent exp) 1)))
                                             (deriv (base exp) var)))                           
        (else (error "unknown expression type -- DERIV" exp))))


(define (exponentiation? exp) (and (pair? exp) (eq? (car exp) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))


(define (make-exponentiation b exp)
  (cond ((=number? b 0) 0)
        ((=number? b 1) 1)
        ((=number? exp 0) 1)
        ((=number? exp 1) b)
        ((and (number? b) (number? exp)) (expt b exp))
        (else (list '** b exp))))




(display "(deriv '(** x 3) 'x) = ")
(deriv '(** x 3) 'x)
(display "(deriv '(** (* x (+ y 2)) 5) 'x) = ")
(deriv '(** (* x (+ y 2)) 5) 'x)




(newline)
