#lang sicp
(#%require (file "stream-utils.rkt"))

#|
A total of (max 0, (- n 1)) additions are performed when using this definition of fibs to calculate the nth fibonacci number:
|#

(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

#|
If we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩), without using the optimization provided by the memo-proc procedure, the number of additions required to compute each value in the stream are:

_ _ 1 1 2 3 5  8  13 21 ... = (stream-cdr fibs)
_ _ 0 1 1 2 3  5  8  13 ... = fibs
0 1 1 2 3 5 8  13 21 34 ... = fibs
0 0 1 2 4 7 12 20 33 54 ... = number of additions

At fib n, the number of additions required A_n = A_{n-1} + A_{n-2} + 1, where A_0 and A_1 = 0.
That is, the number of additions required to compute f(n) is equal to the sum of the number of additions required to compute the previous two fibs, plus one more to add those two terms together.
It turns out that this is equivalent to fib(n) - 1 additions.
|#
