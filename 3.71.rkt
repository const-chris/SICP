#lang sicp
(#%require (file "stream-utils.rkt"))
(#%require (file "3.70.rkt"))

(define (sum-cubes i j)
  (define (cube x) (* x x x))
  (+ (cube i)
     (cube j)))

(define (eq-prev? initial)
  (let ((prev initial))
    (lambda (at)
      (let ((same? (= at prev)))
        (set! prev at)
        same?))))

(define sums-cubes
  (stream-map (lambda (p) (sum-cubes (car p) (cadr p)))
              (weighted-pairs integers integers sum-cubes)))

(define ramanujan-numbers
  (stream-filter (eq-prev? (stream-car sums-cubes))
                 (stream-cdr sums-cubes)))

(stream-take 6 ramanujan-numbers)
