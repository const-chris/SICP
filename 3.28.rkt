#lang sicp
(#%require (file "circuit/wires.rkt"))
(#%require (file "circuit/agenda.rkt"))


(define or-gate-delay 5)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


(define (logical-or x y)
  (cond ((not (and (or (= x 0) (= x 1))
                   (or (= y 0) (= y 1))))
         (error ("Invalid signal -- LOGICAL-OR")))
        ((or (= x 1) (= y 1)) 1)
        (else 0)))




;; tests
(display "(logical-or 0 0) = ")
(logical-or 0 0)

(display "(logical-or 0 1) = ")
(logical-or 0 1)

(display "(logical-or 1 0) = ")
(logical-or 1 0)

(display "(logical-or 1 1) = ")
(logical-or 1 1)
