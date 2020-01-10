#lang racket

;; REVISIT -- import/implement primitives and test

(define (ripple-carry-adder As Bs Ss C)
  (define (iter As Bs Ss C)
    (if (null? As)
        'ok
        (let ((C-next (make-wire)))
          (full-adder (car As) (car Bs) C (car Ss) C-next)
          (iter (cdr As) (cdr Bs) (cdr Ss) C-next))))
  (iter As Bs Ss C))

#|
The time to required for a half-adder to propagate a signal to S is:

(+ and-gate-delay
   (max or-gate-delay
        (+ inverter-delay
           and-gate-delay)))

and to C:

and-gate-delay

the time required for a full-adder to propagate a singal depends only
on the time for the signal to reach c-out, that is:

(+ half-adder-to-S-delay
   half-adder-to-C-delay
   or-gate-delay)

which is:

(+ and-gate-delay
   and-gate-delay
   or-gate-delay
   (max or-gate-delay
        (+ inverter-delay
           and-gate-delay)))

So the delay needed to obtain the complete output from an n-bit ripple-carry-adder
is approximately:

(* n
  (+ and-gate-delay
     and-gate-delay
     or-gate-delay
     (max or-gate-delay
          (+ inverter-delay
             and-gate-delay))))

More precisely, since we don't care about the C-n, the delay is:

(+ (* (- n 1)
      (+ and-gate-delay
         and-gate-delay
         or-gate-delay
         (max or-gate-delay
              (+ inverter-delay
                 and-gate-delay))))
    (+ and-gate-delay
       (max or-gate-delay
            (+ inverter-delay
               and-gate-delay))))

or equivalently:

(- (* n
      (+ and-gate-delay
         and-gate-delay
         or-gate-delay
         (max or-gate-delay
              (+ inverter-delay
                 and-gate-delay))))
   and-gate-delay
   or-gate-delay)
