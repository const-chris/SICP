#lang racket
(require "generic-arithmetic.rkt")
(require "coercion.rkt")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

#|
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
;|#

#|
a) In Louis's system if we were to call a apply-generic with two arguments of type scheme-number or two arguments of type complex
for an operation that is not found in the table for those types, the process would never terminate. It would fail to find the operation
in the table, coerce the first argument to its own type, call apply-generic again with the same arguments as previously, etc.
;|#

#|
(define (exp x y) (apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (cons 'scheme-number (expt x y))))

(exp (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 1 4))
;|#

#|
b) Louis is needlessly worried, apply-generic works fine as is.

c) apply-generic such that it doesn’t try coercion if the two arguments have the same type:
;|#

(define (apply-generic-2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                         (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;#|
(define (exp x y) (apply-generic-2 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (cons 'scheme-number (expt x y))))

(exp (make-complex-from-real-imag 2 3) (make-complex-from-real-imag 1 4))
;|#