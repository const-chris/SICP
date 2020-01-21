#lang sicp
(#%require (file "stream-utils.rkt"))
(#%require (file "3.59.rkt"))
(#%require (file "3.60.rkt"))
(#%require (file "3.61.rkt"))
(#%provide div-series
 tangent-series)

(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
    (error "denominator stream has zero constant term" s2)
    (mul-series s1
                (scale-stream (invert-unit-series s2)
                              (/ 1 (stream-car s2))))))

(define tangent-series
  (div-series sine-series cosine-series))

;; test
#|
(display "(stream-take 10 tangent-series) = ")
(stream-take 10 tangent-series)
;|#
