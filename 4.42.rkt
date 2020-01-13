#lang racket
;; REVISIT -- test with serializer implementation

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else
               (error "Unknown request: MAKE-ACCOUNT"
                      m))))
      dispatch)))

(define acc (make-account 100))
(define withdraw (acc 'withdraw))

(parallel-execute
 (lambda () (withdraw 10))
 (lambda () (withdraw 20)))

(acc 'balance)

#|
Without yet knowing how make-serializer and parallel-execute really work, it is difficult
to say for sure whether this is safe.
The difference between this version and the original is that here each call to withdraw is a
call to the exact same serialized procedure (in the original a new serialized procedure was
generated for each call). If the procedure returned by (make-serializer) is not a pure
function, there could be shenanigans.
I suspect it is safe, however, based on the specification given in the text, since a
serialized procedure must be in the same set as itself, so no additional concurrency should
be allowed by this Ben's version.
|#