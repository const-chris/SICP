#lang sicp
(newline)

(define us-coins (list 25 50 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0 ) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))


(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)


(display "(cc 100 us-coins) = ")
(cc 100 us-coins)


(display "
The order of the coins does not matter, because the procedure evolves a recursive process that visits every combination of coins whose sum is less than or equal to the amount.

")