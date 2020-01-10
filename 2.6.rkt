#lang sicp
(newline)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))




; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f (((lambda (p) (lambda (y) y)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (y) y) x))))
; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))




; (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f (((lambda (p) (lambda (y) (p y))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (y) (f y)) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))




(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))


(define (* a b)
  (lambda (f) (lambda (x) ((a (b f)) x))))




(define (arabic n)
  ((n inc) 0))




(display "(+ two two) = ") 
(arabic (+ two two))

(display "(* two (+ one two)) = ")
(arabic (* two (+ one two)))

; FOR FUTURE: SUBTRACTION AND DIVISION




(newline)


