#lang racket
(require "generic-arithmetic-tower.rkt")

#|
(define (install-project-package)
  (define (project-rat x) (make-integer (car x)))
  (define (project-real x) (make-rational (round x) 1))
  (define (project-complex x) (make-real (real-part x)))
  (put 'project '(rational) project-rat)
  (put 'project '(real) project-real)
  (put 'project '(complex) project-complex)
  'done)


(define (project x)
  (if (get 'project (list (type-tag x)))
      (apply-generic 'project x)
      false))

(define (drop x)
  (let ((projection (project x)))
    (if (and projection (equ? (raise projection) x))
        (drop projection)
        x)))


(install-project-package)
;|#

#|
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((raise1 (raise a1))
                      (raise2 (raise a2)))
                  (cond (raise1
                         (drop (apply-generic op raise1 a2)))
                        (raise2
                         (drop (apply-generic op a1 raise2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
|#
(newline)




;#|
(display "(drop (make-integer 3))                  = ")
(drop (make-integer 3))

(display "(drop (make-rational 3 1))               = ")
(drop (make-rational 3 1))

(display "(drop (make-real 3.0))                   = ")
(drop (make-real 3.0))

(display "(drop (make-complex-from-real-imag 3 0)) = ")
(drop (make-complex-from-real-imag 3 0))

(display "(drop (raise (make-real 0.434235)))      = ")
(drop (raise (make-real 0.434235)))
;|#


;#|
(define c1 (make-complex-from-real-imag 3 -1))
(display "\nc1          = ")
c1

(define c2 (make-complex-from-real-imag 2 1))
(display "c2          = ")
c2

(display "(add c1 c2) = ")
(add c1 c2)
;|#




(newline)
