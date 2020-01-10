#lang racket
(require "symbolic-differentiation.rkt")
(newline)


(define (every pred items)
  (cond ((null? items) true)
        ((not (pred (car items))) false)
        (else (every pred (cdr items)))))

(define (some pred items)
  (cond ((null? items) false)
        ((pred (car items)) true)
        (else (some pred (cdr items)))))

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


      

(define (augend exp) (apply make-sum (cddr exp)))
(define (multiplicand exp) (apply make-product (cddr exp)))


(define (make-sum . xs)
  (cond ((null? xs) 0)
        ((every (lambda (x) (number? x)) xs) (apply + xs))
        (else
         (let ((head (car xs))
               (tail (apply make-sum (cdr xs))))
           (cond ((=number? head 0) tail)
                 ((=number? tail 0) head)
                 (else (list '+ head tail)))))))


(define (make-product . xs)
  (cond ((null? xs) 1)
        ((some (lambda (x) (=number? x 0)) xs) 0)
        ((every (lambda (x) (number? x)) xs) (apply * xs))
        (else
         (let ((head (car xs))
               (tail (apply make-product (cdr xs))))
           (cond ((=number? head 1) tail)
                 ((=number? tail 1) head)
                 (else (list '* head tail)))))))


(display "(deriv '(* x y (+ x 3)) 'x) = ")
(deriv '(* x y (+ x 3)) 'x)




(newline)