#lang sicp
(#%provide (all-defined))

(define (id x) x)

(define (const x)
  (lambda (arg . args) x))
