#lang sicp
(#%require (file "utils/square.rkt"))
(#%require (file "stream-utils.rkt"))
(#%require (file "3.55.rkt"))
(#%require (file "3.64.rkt"))




;; unaccelerated sequence
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))


;; faster
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; Sn−1
        (s1 (stream-ref s 1))  ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define faster-ln2-stream
  (euler-transform ln2-stream))


;; fastest
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define fastest-ln2-stream
  (accelerated-sequence euler-transform ln2-stream))




;; tests
;#|
(define (convergence-count s tolerance)
  (define (iter s count)
    (let* ((tail (stream-cdr s))
           (e1 (stream-car s))
           (e2 (stream-car tail)))
      (if (< (abs (- e1 e2))
             tolerance)
        count
        (iter tail (+ count 1)))))
  (iter s 0))


(display "(stream-take 10 ln2-stream) = ")
(stream-take 10 ln2-stream)
(newline)

(display "(stream-take 10 faster-ln2-stream) = ")
(stream-take 10 faster-ln2-stream)
(newline)

(display "(stream-take 10 fastest-ln2-stream) = ")
(stream-take 10 fastest-ln2-stream)
(newline)
(newline)




(define tolerance 0.0005)
(display "tolerance = ")
tolerance

(display "(convergence-count ln2-stream tolerance) = ")
(convergence-count ln2-stream tolerance)

(display "(convergence-count faster-ln2-stream tolerance) = ")
(convergence-count faster-ln2-stream tolerance)

(display "(convergence-count fastest-ln2-stream tolerance) = ")
(convergence-count fastest-ln2-stream tolerance)
;|#
