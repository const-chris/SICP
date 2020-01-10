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

(define (one-of? x items)
  (some (λ(n) (eq? x n)) items))

(define (take-while pred items)
  (cond ((null? items) '())
        ((not (pred (car items))) '())
        (else (cons (car items) (take-while pred (cdr items))))))

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

(define (make-sum . xs)
  (cond ((null? xs) 0)
        ((every (λ(x) (number? x)) xs) (apply + xs))
        (else
         (let ((head (car xs))
               (tail (apply make-sum (cdr xs))))
           (cond ((=number? head 0) tail)
                 ((=number? tail 0) head)
                 (else (list head '+ tail)))))))

(define (make-product . xs)
  (cond ((null? xs) 1)
        ((some (λ(x) (=number? x 0)) xs) 0)
        ((every (λ(x) (number? x)) xs) (apply * xs))
        (else
         (let ((head (car xs))
               (tail (apply make-product (cdr xs))))
           (cond ((=number? head 1) tail)
                 ((=number? tail 1) head)
                 (else (list head '* tail)))))))




(define (variable? x)
  (and (symbol? x)
       (not (one-of? x '(+ - * / **))))) 

(define (sum? exp)
  (and (pair? exp)
       (pair? (cdr exp))
       (some (λ(x) (eq? x '+))
             (cdr exp))))

(define (addend exp)
  (let ((a
         (take-while (λ(x) (not (eq? x '+)))
                     exp)))
    (if (null? (cdr a))
        (car a)
        a)))

(define (augend exp)
  (if (= (length exp) 3)
      (caddr exp)
      (cddr exp)))

(define (product? exp)
  (and (pair? exp)
       (pair? (cdr exp))
       (eq? (cadr exp) '*)))

(define (multiplier exp) (car exp))

(define (multiplicand exp)
  (let ((product
         (take-while (λ(x)
                       (or (number? x)
                           (variable? x)
                           (pair? x)
                           (eq? x '*)))
                     exp)))
    (if (null? (cdddr product))
        (caddr product)
        (cddr product))))




(display "(deriv '(x + 3 * (x + y + 2)) 'x) = ")
(deriv '(x + 3 * (x + y + 2)) 'x)

(display "(deriv '(x * 3 + (x + y + 2)) 'x) = ")
(deriv '(x * 3 + (x + y + 2)) 'x)




(newline)