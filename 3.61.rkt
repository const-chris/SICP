#lang sicp
(#%require (file "stream-utils.rkt"))
(#%require (file "3.59.rkt"))
(#%require (file "3.60.rkt"))
(#%provide invert-unit-series)

(define (invert-unit-series s)
  (cons-stream 1
               (mul-series (invert-unit-series s)
                           (scale-stream (stream-cdr s) -1))))

;; test
#|
(display
  "(stream-take 5 (mul-series (invert-unit-series cosine-series)
                              cosine-series)) = ")
(stream-take 5 (mul-series (invert-unit-series cosine-series)
                           cosine-series))
;|#
