#lang sicp
(newline)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))


(define seq1 (list 1 2 3 4 5))
(display "seq1 = ")
seq1

(define seq2 (list 6 7 8 9))
(display "seq2 = ")
seq2
(newline)





(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))

(display "(map inc seq1)     = ")
(map inc seq1)




(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(display "(append seq1 seq2) = ")
(append seq1 seq2)




(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(display "(length seq1)      = ")
(length seq1)
(display "(length seq2)      = ")
(length seq2)




(newline)