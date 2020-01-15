#lang racket

#|
Ben is correct, and Louis' worries are unfounded. As long as
  a) individual account transactions are serialized
  b) the balance in from-account is guaranteed tp be greater than or equal
     to amount (non-trivial to guarantee)
|#

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

#|
is safe.

The essential difference between dealing with transfers and dealing with
exchanges is that with exchanges, there was an expectation that the values
being exchanged were invariant. This was not possible to guarantee without
somehow serializing the withdrawal from one account with the deposit into
another.

With transfers on the other hand, there is no expectation that there are
a limited set of "true" values for the account balances. The only
constraint is that the total amount of money in the system (the sum of the
balances) is constant under transfers. As long as individual account
transactions are serialized, this is guaranteed, as shown in the answer to
the previous problem.
|#