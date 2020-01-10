#lang racket
(require "generic-arithmetic.rkt")
(require "coercion.rkt")
(require "2.83.rkt")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((raise1 (raise a1))
                      (raise2 (raise a2)))                   
                  (cond (raise1
                         (apply-generic op raise1 a2))
                        (raise2
                         (apply-generic op a1 raise2))
                        (else
                         (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


; shadow definition of add from generic-arithmetic so that add uses new apply-generic
(define (add x y) (apply-generic 'add x y))

(add (make-rational 1 2) (make-rational 2 3))
(add (make-integer 3) (make-rational 3 4))
(add (make-real 4.2) (make-complex-from-real-imag 4 12))