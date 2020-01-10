#lang sicp
(newline)

(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))




(define seq (list 1 2 3))
(display "seq = ")
seq

(display "(fold-right / 1 seq) = ")
(fold-right / 1 seq)

(display "(fold-left / 1 seq)  = ")
(fold-left / 1 seq)


(display "(fold-right list nil seq) = ")
(fold-right list nil seq)

(display "(fold-left list nil seq)  = ")
(fold-left list nil seq)




(define (op1 a b)
  (- (* a b)
     (+ a b)))

(display "

let (op1 a b) := (- (* a b) (+ a b))

This operation is clearly commutative, but it is not associative:
(op1 (op1 1 2) 3) = (op1 -1 3) = -5
(op1 1 (op1 2 3)) = (op1 1 1) = -1

(fold-right op1 0 seq) = ")
(fold-right op1 0 seq)

(display "(fold-left op1 0 seq) = ")
(fold-left op1 0 seq)

(display "
So commutativity is not sufficient to guarantee that fold-right
and fold-left will produce the same result for any sequence.

<op> must satisfy the associative property to guarantee that fold-right
and fold-left will produce the same result for any sequence

")
