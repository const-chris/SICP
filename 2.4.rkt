#lang sicp
(newline)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))




; To be a valid representation of pairs, these three procedures must satisfy the conditions:
; (car (cons x y)) = x
; (cdr (cons x y)) = y

; By substitution,
; (car (cons x y)) = (car (lambda (m) (m x y)))
;                  = ((lambda (m) (m x y)) (lambda (p q) p))
;                  = ((lambda (p q) p) x y)
;                  = x
(display "(car (cons 1 0)) = ")
(car (cons 1 0))

; Similarly,
; (cdr (cons x y)) = (cdr (lambda (m) (m x y)))
;                  = ((lambda (m) (m x y)) (lambda (p q) q))
;                  = ((lambda (p q) q) x y)
;                  = y
(display "(cdr (cons 1 0)) = ")
(cdr (cons 1 0))




(newline)