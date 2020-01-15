#lang racket
#|
If exchange procedures run sequentially, we can treat each one as an
atom--that is, as a pure function of two arguments which returns a pair
of outputs. Such an atomic exchange function maps each of its operands
from one element of the set of current balances to another element of
the same set. The set of current balances, therefore, is closed under
sequential exchanges.
|#


;; Unserialized exchange procedure
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
#|
If the unserialized exchange procedure is used, situations like this can
occur:

--------------------------------------------------------------------------------------
Peter                  acc1           acc2           acc3           Paul
                       $10            $20            $30
                                                                    access acc1
access acc1
access acc2
difference = -10
exchange acc1 acc2     $20            $10
                                                                    access acc3
                                                                    difference = -20
                                                                    exchange acc1 acc3
                       $40                           $10
--------------------------------------------------------------------------------------

However, as in the example above, the sum of the three balances will be
preserved, even in the event of interleaved exchange procedures, because
no matter what difference is calculated, this same difference is deducted
from one account and added to another, as long as the transactions on
individual account are serialized.
|#


;; make-account with unserialized withdraw and deposit procedures
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) balance)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

#|
If transactions on individual accounts are not serialized (as above), we
can get into situations like this:

----------------------------------------------------------------------------------------------
Peter                          acc1           acc2           acc3           Paul
                               $10            $20            $30
                                                                            access acc3
access acc2  
access acc1
difference = 10
inside (exchange acc1 acc2):
  ((acc2 'withdraw) 10)                       $10
                                                                            access acc1
  inside ((acc1 'deposit) 10):
    access balance
    new val = $20
                                                                            difference = -20
                                                                            exchange acc1 acc3
                               $30                           $10
    set! balance               $20
----------------------------------------------------------------------------------------------
|#