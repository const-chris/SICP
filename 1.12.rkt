#lang racket
(newline)




(define (pascal n)
  (define (position-iter sum count)
    (if (>= sum n)
        (values (- count 1)
                (- count 1 (- sum n)))
        (position-iter (+ sum count) (+ count 1))))

  (define-values (row position-in-row) (position-iter 0 1))
    
  (define above-left (- n row))
        
  (define above-right (+ above-left 1))
  
  (define edge?
    (or (= position-in-row 1)
        (= position-in-row row)))
  
  (cond ((< n 1) "argument must be >= 1")
        (edge? 1)
        (else (+ (pascal above-left)
                 (pascal above-right)))))




(display "(pascal 26) = ")
(pascal 26)




(newline)
