#lang sicp
(#%require (file "stream-utils.rkt"))
(#%provide pairs)

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))




(define iis (pairs integers integers))
#| (stream-take 30 iis) |#

#|
For pairs (x, y), the first appearance of (n, _), (n, n) occurs at the ((2^n) - 1)th element.
Elements beginning with n appear in the order (n, n), (n, n+1), (n, n+2) ...
The interval between (n, n) and (n, n+1) is 2^(n-1) elements.
All subsequent elements of the series (n, n+1), (n, n+2), ... appear at intervals of 2^n elements.

Based on these statements, we can deduce:
(1, 100) appears at element 100 * 2 - 2 = 198 (index 197)
(100, 100) appears at element 2^100 - 1 (index 2^100 - 2)
(99, 100) appears at element 2^99 - 1 + 2^98 (index 2^99 - 2 + 2^98)
|#

;; tests
#|
(newline)

(stream-ref iis 197)

;; testing for (10, 10), since (100, 100) would take a long time to run
(stream-ref iis 1022)

;; testing for (9, 10), since (99, 100) would take a long time to run
(stream-ref iis (+ (expt 2 9) (expt 2 8) -2))
;|#
