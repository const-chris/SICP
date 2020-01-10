#lang sicp

(define (any p items)
  (cond ((null? items) false)
        ((p (car items)) true)
        (else (any p (cdr items)))))

(define (cycle? s)
  (let ((seen '()))
    (define (traverse x)
      (cond ((not (pair? x)) false)
            ((any (lambda (y) (eq? y x)) seen) true)
            (else
             (set! seen (cons x seen))
             (or (traverse (car x))
                 (traverse (cdr x))))))
    (traverse s)))



;; tests
(define x '(1 2 1 4))
(display "x = ")
x

(display "(cycle? x) = ")
(cycle? x)

(newline)


(set-car! (cddr x) (cdr x))
(display "(set-car! (cddr x) (cdr x)) -> ")
x

(display "(cycle? x) = ")
(cycle? x)