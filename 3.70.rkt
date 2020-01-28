#lang sicp
(#%require (file "stream-utils.rkt"))
(#%provide weighted-pairs)

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let* ((s1car (stream-car s1))
                 (s2car (stream-car s2))
                 (w1 (apply weight s1car))
                 (w2 (apply weight s2car)))
            (if (< w1 w2)
                (cons-stream
                  s1car
                  (merge-weighted (stream-cdr s1)
                                  s2
                                  weight))
                (cons-stream
                  s2car
                  (merge-weighted s1
                                  (stream-cdr s2)
                                  weight)))))))


(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))


;; a)
(define integer-pairs-by-sum (weighted-pairs integers integers +))
#| (stream-take 20 integer-pairs-by-sum) |#

;; b)
(define (w i j)
  (+ (* 2 i)
     (* 3 j)
     (* 5 i j)))

(define (f  p)
  (let ((i (car p))
        (j (cadr p)))
    (not (or (= 0 (remainder i 2))
             (= 0 (remainder i 3))
             (= 0 (remainder i 5))
             (= 0 (remainder j 2))
             (= 0 (remainder j 3))
             (= 0 (remainder j 5))))))

(define integer-pairs-by-w (stream-filter f (weighted-pairs integers integers w)))
#| (stream-take 20 integer-pairs-by-w) |#
