#lang sicp
(#%require (file "stream-utils.rkt"))
(#%provide cosine-series
 sine-series)

(define (recip x) (/ 1 x))


;; a)
(define (integrate-series s)
  (mul-streams s (stream-map recip integers)))

;; b)
;; cosine is equal to the integral of the negative of sine, plus the constant term, cos(0) = 1
(define cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

;; sine is equal to the integral of cosine, plus the constant term, sin(0) = 0
(define sine-series (cons-stream 0 (integrate-series cosine-series)))




;; test
#|
(display "(stream-take 6 cosine-series) = ")
(stream-take 6 cosine-series)

(display "(stream-take 6 sine-series)   = ")
(stream-take 6 sine-series)
;|#
