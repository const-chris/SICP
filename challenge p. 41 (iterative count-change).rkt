#lang scheme

; Iterative count-change

(define denominations '(5 1 10 50 25))



; fourth attempt, verifying time-complexity
; this is actually slower, even though it does no repeated computation
(define (cc amount)
  (define (inc-count? count nickels dimes quarters half-dollars)
    ; (display (string-append (number->string nickels) " " (number->string dimes) " " (number->string quarters) " " (number->string half-dollars)))
    ;  (newline)
    (if (<= (+ (* 5 nickels)
               (* 10 dimes)
               (* 25 quarters)
               (* 50 half-dollars))
            amount)
        (+ count 1)
        count))

  (define (max-of-denom denom sum)
    (quotient (- amount sum) denom))
  
  (define (cc-iter count nickels max-nickels dimes max-dimes quarters max-quarters half-dollars max-half-dollars)
    (define dime-sum (* 10 dimes))
    (define quarter-sum (* 25 quarters))
    (define half-dollar-sum (* 50 half-dollars))
    (cond ((and (>= half-dollars max-half-dollars)
                (>= quarters max-quarters)
                (>= dimes max-dimes)
                (>= nickels max-nickels))
           (inc-count? count nickels dimes quarters half-dollars))
          ((and (>= quarters max-quarters)
                (>= dimes max-dimes)
                (>= nickels max-nickels))
           (cc-iter (inc-count? count nickels dimes quarters half-dollars)
                    0
                    (max-of-denom 5 (+ half-dollar-sum 50))
                    0
                    (max-of-denom 10 (+ half-dollar-sum 50))
                    0
                    (max-of-denom 25 (+ half-dollar-sum 50))
                    (+ half-dollars 1)
                    (quotient amount 50)))
          ((and (>= dimes max-dimes)
                (>= nickels max-nickels))              
           (cc-iter (inc-count? count nickels dimes quarters half-dollars)
                    0
                    (max-of-denom 5 (+ quarter-sum half-dollar-sum 25))
                    0
                    (max-of-denom 10 (+ quarter-sum half-dollar-sum 25))
                    (+ quarters 1)
                    (max-of-denom 25 half-dollar-sum)
                    half-dollars
                    (quotient amount 50)))
          ((and (>= nickels max-nickels))                        
           (cc-iter (inc-count? count nickels dimes quarters half-dollars)
                    0
                    (max-of-denom 5 (+ dime-sum quarter-sum half-dollar-sum 10))
                    (+ dimes 1)
                    (max-of-denom 10 (+ quarter-sum half-dollar-sum))
                    quarters
                    (max-of-denom 25  half-dollar-sum)
                    half-dollars
                    (quotient amount 50)))
          (else          
           (cc-iter (inc-count? count nickels dimes quarters half-dollars)
                    (+ nickels 1)
                    (max-of-denom 5 (+ dime-sum quarter-sum half-dollar-sum))
                    dimes
                    (max-of-denom 10 (+ quarter-sum half-dollar-sum))
                    quarters
                    (max-of-denom 25  half-dollar-sum)
                    half-dollars
                    (quotient amount 50)))))
  (cc-iter  0
            0
            (quotient amount 5)
            0
            (quotient amount 10)
            0
            (quotient amount 25)
            0
            (quotient amount 50)))
                         
(cc 3000)




; third draft
(define (count-change amount)
  ((adder-factory denominations) amount 0 0))

(define (adder-factory denominations)
  (define denomination (car denominations))
  (cond ((null? (cdr denominations))
         (λ (amount count sum) (if (= (remainder (- amount sum) denomination) 0)
                                   (+ count 1)
                                   count)))
        (else (define add-next (adder-factory (cdr denominations)))
              (λ (amount count sum)                             
                (define (add-current num-current count)
                  (if (< num-current 0)
                      count
                      (add-current (- num-current 1)
                                   (if (= (+ sum (* num-current denomination)) amount)
                                       (+ count 1)
                                       (add-next amount count (+ sum (* num-current denomination)))))))                                                                                                         
                (add-current (quotient (- amount sum) denomination)
                             count)))))

; (count-change 51)




; second draft (worked as long as denominations included unit coin):
(define (adder-prefactory denominations)
  (cond ((null? (cdr denominations))
         (λ (amount count sum) (+ count 1)))
        (else (define add-next (adder-prefactory (cdr denominations)))
              (λ (amount count sum)
                (define denomination (car denominations))              
                (define (add-current num-current count)
                  (if (< num-current 0)
                      count
                      (add-current (- num-current 1)
                                   (if (= (+ sum (* num-current denomination)) amount)
                                       (+ count 1)
                                       (add-next amount count (+ sum (* num-current denomination)))))))                                                                                                         
                (add-current (quotient (- amount sum) denomination)
                             count)))))




; first draft (fastest, but very rigid):
(define (first-count-change amount)
  (add-half-dollars amount 0 0))

(define (add-half-dollars amount count sum)
  (define max-half-dollars (quotient amount 50))
  (define (add-half-dollars-iter half-dollars count)
    (if (< half-dollars 0)
        count       
        (add-half-dollars-iter (- half-dollars 1)
                               (if (= (+ sum (* half-dollars 50)) amount)
                                   (+ count 1)
                                   (add-quarters amount count (+ sum (* half-dollars 50)))))))
  (add-half-dollars-iter max-half-dollars count))

(define (add-quarters amount count sum)
  (define max-quarters (quotient (- amount sum) 25))
  (define (add-quarters-iter quarters count)
    (if (< quarters 0)
        count
        (add-quarters-iter (- quarters 1)
                           (if (= (+ sum (* quarters 25)) amount)
                               (+ count 1)   
                               (add-dimes amount count (+ sum (* quarters 25)))))))
  (add-quarters-iter max-quarters count))
    
(define (add-dimes amount count sum)
  (define max-dimes (quotient (- amount sum) 10))
  (define (add-dimes-iter dimes count)
    (if (< dimes 0)
        count       
        (add-dimes-iter (- dimes 1)
                        (if (= (+ sum (* dimes 10)) amount)
                            (+ count 1)
                            (add-nickels amount count (+ sum (* dimes 10)))))))
  (add-dimes-iter max-dimes count))
      
(define (add-nickels amount count sum)
  (define max-nickels (quotient (- amount sum) 5))
  (define (add-nickels-iter nickels count)
    (if (< nickels 0)
        count
        (add-nickels-iter (- nickels 1)
                          (if (= (+ sum (* nickels 5)) amount)
                              (+ count 1)
                              (add-pennies amount count (+ sum (* nickels 5)))))))
  (add-nickels-iter max-nickels count))

(define (add-pennies amount count sum)
  (+ count 1))

; (first-count-change 3000)