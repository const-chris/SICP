#lang racket
(require "racket-pairs.rkt")
(provide cons
         list
         car
         cadr
         caddr
         cadddr
         cdr
         cddr
         set-car!
         set-cdr!)




(define (cons x y)
  (lambda (m)
    (cond
      ((eq? m 'car) x)
      ((eq? m 'cdr) y)
      ((eq? m 'set-car!)
       (lambda (val)
         (set! x val)))
      ((eq? m 'set-cdr!) 
       (lambda (val)
         (set! y val)))
      (else
       (error "unknown-message -- CONS" m)))))


(define (list . xs)
  (if (null? xs)
      '()
      (cons (racket-car xs) (apply list (racket-cdr xs)))))


(define (car pair)
  (pair 'car))

(define (cadr pair)
  ((pair 'cdr) 'car))

(define (caddr pair)
  (((pair 'cdr) 'cdr) 'car))

(define (cadddr pair)
  ((((pair 'cdr) 'cdr) 'cdr) 'car))

(define (cdr pair)
  (pair 'cdr))

(define (cddr pair)
  ((pair 'cdr) 'cdr))


(define (set-cdr! pair val)
  ((pair 'set-cdr!) val))

(define (set-car! pair val)
  ((pair 'set-car!) val))




#|
;; tests
(define p (cons 1 2))
p
(car p)
(cdr p)
(set-car! p 3)
(car p)
(set-cdr! p 4)
(cdr p)


(define xs (list 1 2 3 4))
(car xs)
(cdr xs)
(car (cdr xs))
(cadr xs)
(caddr xs)
(cadddr xs)

;|#