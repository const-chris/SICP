#lang racket

(provide accumulate dec enumerate-interval filter flatmap inc nil some sum)

(define nil (list))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))


(define (dec x) (- x 1))


(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start (enumerate-interval (+ start 1) end))))


(define (filter pred seq)
  (accumulate (lambda (x y)
                (if (pred x)
                    (cons x y)
                    y))
              nil
              seq))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (inc x) (+ x 1))


(define (some pred items)
  (cond ((null? items) false)
        ((pred (car items)) true)
        (else (some pred (cdr items)))))


(define (sum items)
  (accumulate + 0 items))


