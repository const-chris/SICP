#lang sicp
#|
The procedures (actions) in each time segment in the agenda must be processed in the order in which they were
added to the agenda in order to track the state changes encapsulated by the action procedures that generated
them.

When set-signal! is called on a wire, its value is updated and then its action procedures are called. The
action procedures encapsulate the state of the wires attached to their action box at this time. The action
added to the agenda by each action procedure referenced this state, no matter the order in which agenda actions
are called, so it is important that they are called in the same order in which the states they reference
existed.

For example, an and-gate adds two procedures to the agenda in the same time segment, one corresponding to
each input.

If the and-gate's inputs are changed from 0, 1 to 1, 0 in the same time segment, in the correct world (using a
queue), the first action added -- corresponding to changing a1's signal from 0 to 1 -- happens first. This
and-action-procedure sees both a1 and a2 as 1, so it sets the output signal to 1. Immediately after that the
second action added -- corresponding to changing a2's signal from 1 to 0 -- fires. This second and-action-
procedure sees a1's value as 1 and a2's value as 0, so it sets the output signal to 0, which is what we want.

In the borked world where we use a stack instead of a queue in the agenda's time segments (see below), the action
procedures happen in the same order: The value of a1 is changed to 1, and and-action-procedure adds an action
to the agenda which sees the input state as 1, 1. Then the value of a2 is changed to 0, and and-action-procedure
adds an action to the agenda which sees the input state as 1, 0. So far so good. But these actions happen in the
reverse order. The second action added happens first. It sees the input values as 1, 0, so the output is set to 0
(no change, and so far a hidden bug). Then the first action added is processed. When it was added to the agenda,
a2's value was still 1, and a1's had just been changed to 1, so the output is set to 1. Calamity! The bug rears
its ugly head.
|#


(#%require (file "stack.rkt"))
(#%provide after-delay
           propagate
           current-time
           the-agenda)


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

;; stack instead of queue!
(define (segment-stack s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


;; stack implementation... oh no!
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((s (make-stack)))
      (push! s action)
      (make-time-segment time s)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (push! (segment-stack (car segments))
               action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((s (segment-stack (first-segment agenda))))
    (pop! s)
    (if (empty-stack? s)
        (set-segments! agenda (rest-segments agenda))
        'nothing)))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (peek (segment-stack first-seg)))))

(define the-agenda (make-agenda))

















