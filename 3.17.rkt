#lang sicp
;(require "utils/any.rkt")
(newline)

(define (any p items)
  (cond ((null? items) false)
        ((p (car items)) true)
        (else (any p (cdr items)))))

(define (count-pairs s)
  (let ((seen '()))
    (define (traverse x)
      (if (or (not (pair? x))
              (any (lambda (y) (eq? y x)) seen))
          0
          (begin
            (set! seen (cons x seen))
            (+ (traverse (car x))
               (traverse (cdr x))
               1))))
    (traverse s)))




;; tests
(define x '(1 2 3))
(display "x = ")
x

(display "(count-pairs x) = ")
(count-pairs x)

(newline)


(set-cdr! (cddr x) x)
(display "(set-cdr! (cddr x) x) -> ")
x

(display "(count-pairs x) = ")
(count-pairs x)




(newline)