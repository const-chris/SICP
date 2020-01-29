#lang sicp
(#%require (file "stream-utils.rkt"))
(#%provide sense-data sign-change-detector)

;; sample-data
(define (make-stream . xs)
  (if (null? xs)
      '()
      (cons-stream (car xs) (apply make-stream (cdr xs)))))

(define sense-data (make-stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))




;; Not supplied: sign-change-detector
(define (sign-change-detector at prev)
  (cond ((and (> at 0) (< prev 0)) 1)
        ((and (< at 0) (> prev 0)) -1)
        (else 0)))


;; Alyssa's system
(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))

(define zero-crossings
  (make-zero-crossings sense-data 0))

;; Eva Lu's suggestion
(define zero-crossings-2
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))




;; tests
#|
(stream-take 12 zero-crossings)
(stream-take 12 zero-crossings-2)
;|#
