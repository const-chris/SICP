#lang sicp
(#%require (file "stream-utils.rkt"))
(#%require (file "3.74.rkt"))

;; Louis' version
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
      (sign-change-detector avpt last-value)
      (make-zero-crossings
        (stream-cdr input-stream) avpt))))

;; Louis' mistake is intermingling the smoothed and unsmoothed streams. To generate the smoothed stream, he should average the current input-stream value with the previous input-stream value, but he averages the current input-stream value with the last smoothed-stream value. To fix this, we need to keep track of the previous input-stream value as well as the previous smoothed-stream value.


;; Less-borked version
(define (better-make-zero-crossings input-stream last-input last-smoothed)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-input)
                 2)))
    (cons-stream
      (sign-change-detector avpt last-smoothed)
      (better-make-zero-crossings
        (stream-cdr input-stream) (stream-car input-stream) avpt))))




;; test
#| (stream-take 12 (better-make-zero-crossings sense-data 0 0)) |#

