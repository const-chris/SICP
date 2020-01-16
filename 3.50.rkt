#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;; tests
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-take n xs)
  (if (= 0 n)
      '()
      (cons (stream-car xs)
            (stream-take (- n 1) (stream-cdr xs)))))


(define xs (stream-enumerate-interval 1 10))
(define ys (stream-enumerate-interval 11 20))
(define zs (stream-enumerate-interval 21 30))

(stream-take 5 (stream-map + xs ys zs))