#lang sicp
(newline)

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))




(define (dot-product v w)
  (accumulate + 0 (map * v w)))




(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))




(define (transpose m)
  (accumulate-n cons nil m))




(define (matrix-*-matrix m1 m2)
  (let ((cols (transpose m2)))
    (map (lambda (x) (matrix-*-vector cols x)) m1)))




(define A (list (list 1 -1 2) (list 0 -3 1)))
(display "A = ")
A

(define v (list 2 1 0))
(display "v = ")
v
(newline)


(display "(matrix-*-vector A v) = ")
(matrix-*-vector A v)

(display "(transpose A) = ")
(transpose A)
(newline)
(newline)


(define B (list (list 0 4 -2) (list -4 -3 0)))
(display "B = ")
B

(define C (list (list 0 1) (list 1 -1) (list 2 3)))
(display "C = ")
C
(newline)


(display "(matrix-*-matrix B C) = ")
(matrix-*-matrix B C)




(newline)