#lang sicp
(#%require (file "stream-utils.rkt"))
(#%require (file "3.74.rkt"))




(define (smooth input-stream)
  (stream-map (lambda (x1 x2) (/ (+ x1 x2) 2))
              input-stream
              (stream-cdr input-stream)))


(define (make-zero-crossings input-stream)
  (stream-map sign-change-detector
              input-stream
              (stream-cdr input-stream)))


(define zero-crossings (make-zero-crossings (cons-stream 0 (smooth sense-data))))




;; test
#| (stream-take 12 (smooth (cons-stream 0 sense-data))) |#
#| (stream-take 11 zero-crossings) |#
