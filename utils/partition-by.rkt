#lang sicp
(#%provide partition-by)

(define (partition-by f xs)
  (letrec ((partition
             (lambda (xs continue)
               (if (null? xs)
                   (continue '() '())
                   (let ((x (car xs)))
                     (partition (cdr xs)
                                (lambda (left right)
                                  (if (f x)
                                      (continue (cons x left) right)
                                      (continue left (cons x right))))))))))
    (partition xs cons)))

#| (partition-by odd? '(1 2 3 4 5)) |#
