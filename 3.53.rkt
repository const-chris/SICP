#lang sicp
(#%require (file "stream-utils.rkt"))

(define s (cons-stream 1 (add-streams s s)))

#|
s = 1 2 4 8 ...
i.e. it is a stream of powers of 2, beginning with 2^0
|#

;; proof
(stream-take 10 s)
