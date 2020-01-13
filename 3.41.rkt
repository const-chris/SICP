#lang racket
;; REVISIT -- test with serializer implementation

;; Original
(define (make-account-1 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))


(define account (make-account-1 100))
(define withdraw (account 'withdraw))
(define balance (account 'balance))


;; contrived example
(define (partial-withdraw n)
  (let ((amount-to-withdraw ((balance) / n)))
    (withdraw amount-to-withdraw)))

(partial-withdraw 2)  ;; P1
(withdraw 90)         ;; P2

#|
possible outcomes:

50: P1 then P2 (P2 unsuccessful because of insufficient funds)
5: P2 then P1

10: P1 accesses balance, then P2, then P1 attempts withdraw,
    but fails because there is a check against the balance inside the
    serialized withdraw
|#


;; Ben's version
(define (make-account-2 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected
               (lambda () balance)))) ; serialized
            (else
             (error "Unknown request: MAKE-ACCOUNT"
                    m))))
    dispatch))


#|
I don't agree that this is necessary. Access alone can't cause problems because it is non-mutating.
And even in the contrived example above, where unserialized access to the balance is used to determine
an amount to withdraw, that amount still must be passed as a parameter to a serialized procedure in
order to move any money, at which point any inconsistencies are uncovered.
|#