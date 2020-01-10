#lang racket
(provide flip)

(define (flip proc)
  (lambda (a b) (proc b a)))

; ((flip -) 3 5)