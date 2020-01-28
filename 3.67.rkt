#lang sicp
(#%require (file "stream-utils.rkt"))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (interleave
        (stream-map (lambda (x) (list x (stream-car t)))
                    (stream-cdr s))
        (pairs (stream-cdr s) (stream-cdr t))))))


;; test
(stream-take 100 (pairs integers integers))
