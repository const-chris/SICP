#lang racket
(require "table.rkt")

(define-values (get put print-table) (make-table))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY GENERIC" (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))  ; unnecessary, but harmless
  (put 'add
       '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub
       '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul
       '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div
       '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make
       'scheme-number
       (lambda (x) (tag x)))                      ; unnecessary, but harmless
  'done)




(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag z)
  (cond ((number? z) 'scheme-number)
        ((pair? z) (car z))
        (else
         (error "Bad tagged datum -- TYPE-TAG" z))))

(define (contents z)
  (cond ((number? z) z)
        ((pair? z) (cdr z))
        (else
         (error "Bad tagged datum -- CONTENTS" z))))




(install-scheme-number-package)


(display "\n(add 5 -1) = ")
(add 5 -1)




(newline)
