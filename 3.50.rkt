#lang sicp
(#%require (file "stream-utils.rkt"))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;; tests
(define xs (stream-enumerate-interval 1 10))
(define ys (stream-enumerate-interval 11 20))
(define zs (stream-enumerate-interval 21 30))

(stream-take 5 (stream-map + xs ys zs))
