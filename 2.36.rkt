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




(define seq1 (list 1 2 3))
(define seq2 (list 4 5 6))
(define seq3 (list 7 8 9))
(define seq4 (list 10 11 12))
(define s (list seq1 seq2 seq3 seq4))
(display "s = ")
s

(display "(accumulate-n + 0 s) = ")
(accumulate-n + 0 s)




(newline)