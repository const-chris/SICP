#lang racket
(newline)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password _)
    "Incorrect password")
  (define (dispatch p m) 
    (cond ((not (eq? p password)) incorrect-password)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)



(define acc (make-account 100 'secret-password))
(display "(define acc (make-account 100 'secret-password))\n\n")

(display "((acc 'secret-password 'withdraw) 40)     -> ")
((acc 'secret-password 'withdraw) 40)


(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)




(newline)