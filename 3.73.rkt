#lang sicp
(#%require (file "stream-utils.rkt"))


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)




(define (RC R C dt)
  (lambda (i v0)
    (add-streams (integral (scale-stream i (/ 1 C)) v0 dt)
                 (scale-stream i R))))


;; test
;#|
(define RC1 (RC 5 1 0.5))
(stream-take 10 (RC1 ones 1))
;|#
