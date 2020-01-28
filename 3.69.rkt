#lang sicp
(#%require (file "utils/square.rkt"))
(#%require (file "stream-utils.rkt"))
(#%require (file "3.66.rkt"))

(define (triples s t u)
  (let ((p (pairs t u)))
    (cons-stream
      (cons (stream-car s) (stream-car p))
      (interleave
        (stream-map (lambda (x) (cons (stream-car s) x))
                    (stream-cdr p))
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))


(define pythagorean-triples
  (stream-filter (lambda (xs) (= (square (caddr xs))
                                 (+ (square (car xs)) (square (cadr xs)))))
                 (triples integers integers integers)))

;; test
#|
(stream-take 20 (triples integers integers integers))

(stream-take 5 pythagorean-triples)
;|#
