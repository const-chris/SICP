#lang sicp

#|
Here we treat defining y as an expensive side-effect. Without memoization, (factorial 100) is computed
for every element of the list. With memoization, it is computed only once.
|#
(define prog
  '((define (map op xs)
      (if (null? xs)
          '()
          (cons (op (car xs)) (map op (cdr xs)))))

    (define (range start end)
      (cond ((> start end) (range end start))
            ((= start end) '())
            (else (cons start (range (+ start 1) end)))))

    (define (f x)
      (define y (range 0 100))
      x)

    (map f (range 0 100))))


#| tests
#| (#%require (file "./lazy-evaluator-no-memo.rkt")) ;; takes ~100x longer |#
(#%require (file "./lazy-evaluator.rkt"))

(define (run prog)
  (let ((start-time (runtime))
        (res (force-it (eval-sequence prog the-global-environment))))
    (display "running time: ")
    (display (- (runtime) start-time))
    (newline)
    res))

(run prog)
;|#

#|
(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define (square x) (* x x))

without memoization:
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
2

with memoization:
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1
|#
