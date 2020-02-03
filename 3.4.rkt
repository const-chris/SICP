#lang sicp
(newline)

(define (make-account balance password)
  (let ((consec-fails 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password _)
      (string-append "Incorrect password. "
                     (number->string (- 8 consec-fails))
                     (if (= consec-fails 7)
                         " attempt "
                         " attempts ")
                     "remaining."))
    (define (call-the-cops _)
      "Calling the cops.")
    (define (dispatch p m) 
      (if (not (eq? p password))
          (begin
            (set! consec-fails (+ consec-fails 1))
            (if (> consec-fails 7)
                call-the-cops
                incorrect-password))
          (begin
            (set! consec-fails 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: MAKE-ACCOUNT"
                               m))))))
    dispatch))



(define acc (make-account 100 'secret-password))
(display "(define acc (make-account 100 'secret-password))\n\n")

(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'secret-password 'withdraw) 40)     -> ")
((acc 'secret-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(newline)

(display "((acc 'secret-password 'withdraw) 40)     -> ")
((acc 'secret-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)
(display "((acc 'some-other-password 'withdraw) 40) -> ")
((acc 'some-other-password 'withdraw) 40)




(newline)
