#lang sicp
(#%require (file "./stream-utils.rkt"))

;; a)
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map
    stream-car
    (stream-filter
      (lambda (s)
        (not (stream-null? s)))
      stream)))

;; b) Using simple-stream-flatmap in place of stream flatmap in stream-flatmap in negate, lisp-value, and find-assertions does not change the behavior of the query system.
