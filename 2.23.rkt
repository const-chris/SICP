#lang sicp
(newline)

(define (print item)
  (display item)
  (newline))

(define items (list 57 321 88))




(define (for-each proc items)
  (cond ((null? items) true)
        (else (proc (car items))
              (for-each proc (cdr items)))))




(display "(for-each (λ (x)
             (display x)
             (newline))
          (list 57 321 88))

")


(for-each print items)




(newline)