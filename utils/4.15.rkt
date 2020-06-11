#lang sicp

;; Given:
(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))


(try try)
;; is equivalent to
(if (halts? try try)
    (run-forever)
    'halted)

;; Assuming halts? works as intended
;; (halts? p a) == true when (p a) terminates, false otherwise.
;; There are two possibilities:

;; (1) (halted? try try) == true
(if true (run-forever) 'halted)
;; which equals
(run-forever)
;; which never terminates.
;; This is an obvious contradiction, since halted? told us that (try try) terminates, and it didn't.

;; (2) (halted? try try) == false
(if false (run-forever) 'halted)
;; which equals
'halted
;; Again this is a contradiction.
;; halted? tells us that (try try) doesn't terminate, but it did and returned the value 'halted.

;; Therefore, by contradiction, it is not possible to write a procedure halts? that correctly determines
;; whether p halts on a for any procedure p and object a.

