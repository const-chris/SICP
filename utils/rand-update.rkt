#lang racket
(provide rand-update)

(define (rand-update x)
  (let ((m (expt 2 32))
        (a 22695477)
        (c 1))
    (bitwise-bit-field
      (modulo (+ c (* a x)) m) 16 30)))
