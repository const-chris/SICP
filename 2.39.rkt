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




(define (r-reverse seq)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              seq))



(define (l-reverse seq)
  (fold-left (lambda (x y) (cons y x))
             nil
             seq))




(define s (list 1 2 3))
(display "s = ")
s
(newline)


(display "Implemented with fold-right, (reverse s) = ")
(r-reverse s)


(display "Implemented with fold-left,  (reverse s) = ")
(l-reverse s)




(newline)