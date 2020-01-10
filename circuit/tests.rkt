#lang racket
(require "wires.rkt")
(require "primitive-function-boxes.rkt")
(require "agenda.rkt")
;(require "../3.32.rkt") ;; to see this in action, change import in primitive-function-boxes.rkt as well


(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))



#|
(probe 'sum sum)

(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)
;|#

(define res (make-wire))
(probe 'res res)
(and-gate input-1 input-2 res)

(set-signal! input-1 0)
(set-signal! input-2 1)

(propagate)

(set-signal! input-1 1)
(set-signal! input-2 0)

(propagate)




(newline)