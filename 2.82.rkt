#lang racket
(require "generic-arithmetic.rkt")
(require "coercion.rkt")

(define (some pred items)
  (cond ((null? items) false)
        ((pred (car items)) true)
        (else
         (some pred (cdr items)))))

(define (every pred items)
  (define (iter things)
    (cond ((null? things) true)
          ((not (pred (car things))) false)
          (else (iter (cdr things)))))
  (if (null? items)
      false
      (iter items)))




(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
            (else
             (cond ((every (lambda (t) (eq? (car type-tags) t)) type-tags) (error "No method for these types" (list op type-tags))))
             (define (iter current previously-tried untried)
               (define (t->current t) (get-coercion (type-tag t) (type-tag current)))
               (define (same-type? t) (eq? (type-tag current) (type-tag t)))
               (define (coercable-or-same? t) (or (t->current t) (same-type? t)))
               (define (coerce-to-current t)
                 (if (same-type? t)
                     t
                     ((t->current t) t)))
               
               (cond ((every coercable-or-same? (append previously-tried untried))
                      (apply apply-generic op (append (map coerce-to-current previously-tried)
                                                      (list current)
                                                      (map coerce-to-current untried))))
                     ((null? untried) (error "No method for these types" (list op type-tags)))
                     (else
                      (iter (car untried) (append previously-tried (list current)) (cdr untried)))))
             
             (iter (car args) '() (cdr args)))))))




; trivial operation to demonstrate that apply generic works with an arbitrary number of arguments:
(define (display-arg-types . args) (apply apply-generic 'display-arg-types args))
(put 'display-arg-types '(complex complex complex) (lambda (x . xs) (map type-tag (cons x xs))))




; test:
(display-arg-types (make-scheme-number 3)
                   (make-complex-from-real-imag 5 1)
                   (make-complex-from-real-imag 4 1))

#|
This solution is not sufficiently general when, for example, an operation is defined '(complex complex complex), and the arguments are of type
'(scheme-number scheme-number scheme-number). All three arguments could be coerced to complex, but this solution doesn't take that into account.

Similarly, our system requires that arguments be given in the same order as that in which they are defined. In most cases, this is a good thing.
But for commutative operations, we miss opportunities. For example, if add were defined for '(scheme-number complex complex) and we tried to
(add (make-complex-from-real-imag 3 1)
     (make-scheme-number 4)
     (make-complex-from-real-imag 5 1))
it wouldn't work, even though we have the coercions in place to do this calculation. (This is a bit of a silly example, as almost certainly the add
operation would be defined for '(complex complex complex) if it were defined for '(scheme-number complex complex), but it demonstrates the idea.)
;|#

(display-arg-types (make-scheme-number 3)
                   (make-scheme-number 5)
                   (make-scheme-number 4))


