#lang racket
(require "generic-arithmetic.rkt")

(define (magnitude z) (apply-generic 'magnitude z))

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)


(define z (make-complex-from-real-imag 3 4))
(magnitude z)

#|
This works because each time apply-generic is called, it strips off the first tag
and treats the rest the contents.

(magnitude z) -> (apply-generic 'magnitude z)
              -> (apply (get 'magnitude '(complex)) '(rectangular 3 4))
              -> (apply (apply-generic 'magnitude) '(rectangular 3 4))
              -> (apply-generic 'magnitude '(rectangular 3 4))
              -> (apply (get 'magnitude 'rectangular) '(3 4))
              -> (apply (lambda (x) (sqrt (+ (square (real-part x)
                                             (square (imag-part x))))
                        '(3 4))
              -> (sqrt (+ (square (car '(3 4))
                          (square (cdr '(3 4)))))
              -> 5
;|#

