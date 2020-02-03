#lang sicp
(#%require (file "circuit/wires.rkt"))
(#%require (file "circuit/agenda.rkt"))

(define inverter-delay 2)
(define and-gate-delay 3)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and x y)
  (cond ((not (and (or (= x 0) (= x 1))
                   (or (= y 0) (= y 1))))
         (error ("Invalid signal -- LOGICAL-AND")))
        ((and (= x 1) (= y 1)) 1)
        (else 0)))




(define (compound-or x y)
  (not (and (not x) (not y))))

(define (or-gate o1 o2 output)
  (let ((a1 (make-wire))
        (a2 (make-wire))
        (n (make-wire)))
    (inverter o1 a1)
    (inverter o2 a2)
    (and-gate a1 a2 n)
    (inverter n output)
    'ok))

;; tests
(compound-or false false)
(compound-or true false)
(compound-or false true)
(compound-or true true)


;; The delay time for the or-gate is 2 * inverter-delay + and-gate-delay
