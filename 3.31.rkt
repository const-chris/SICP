#lang sicp

#|
If we didn't update the wires' states when we added new procedures to them, we'd end up with nonsensical structures. The most obvious example is probably the input and output wires of an inverter. If we didn't update the wires' states when attaching them to the inverter, both input and output wires would have a signal of 0. This is clearly not what we want.

To demonstrate, if we had defined accept-action-procedure! as:
|#

(define (accept-action-procedure! proc)
  (set! action-procedures (cons proc action-procedures))
  ;; omitting (proc) here
  )

;; When we call

(half-adder input-1 input-2 sum carry)

;; inside half-adder, there is a call

(inverter c e)

#|
In the correct world, this call adds an item to the agenda, and after one inverter-delay the output (e) is inverted. This in turn sends an update signal to the second and-gate in the half-adder, and when input-1's signal is set to 1, and this signal propagates through the half-adder's or-gate and flips wire d's signal to a 1, the second and-gate's inputs are both 1, so it updates the signal on wire s, and the probe 'sum sees the signal 1.

In our borked world where we neglect to initialize wires by running the appropriate action-procedures when they are attached to logic boxes, the agenda item to update wire e is never added to the queue. Wire e's signal stays 0, so when input-1 switches to 1 and this propagates to wire d, the half-adder's second and-gate sees a 1 and a 0, so it doesn't flip the signal on wire s.
;|#
