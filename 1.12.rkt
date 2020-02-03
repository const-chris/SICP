#lang sicp


(define (pascal n)
  (define (position-iter sum count)
    (if (>= sum n)
        (cons (- count 1)
              (- count 1 (- sum n)))
        (position-iter (+ sum count) (+ count 1))))
  (let* ((rp (position-iter 0 1))
         (row (car rp))
         (position-in-row (cdr rp))
         (above-left (- n row))
         (above-right (+ above-left 1))
         (edge? (or (= position-in-row 1)
                    (= position-in-row row))))
    (cond ((< n 1) "argument must be >= 1")
          (edge? 1)
          (else (+ (pascal above-left)
                   (pascal above-right))))))




(display "(pascal 26) = ")
(pascal 26)
