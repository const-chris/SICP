#lang sicp
(#%require (file "utils/square.rkt"))
(newline)

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'reset-count)
             (set! count 0)
             "counter reset")
            ((eq? x 'how-many-calls?) count)
            (else
              (set! count (+ count 1))
              (f x))))))

(define S1 (make-monitored square))
(define S2 (make-monitored square))


(display "S1, S2 = (make-monitored square)\n\n")

(display "(S2 4) = ")
(S2 4)

(display "(S2 3) = ")
(S2 3)

(display "(S1 3) = ")
(S1 3)

(display "(S2 'how-many-calls?) = ")
(S2 'how-many-calls?)

(display "(S2 reset-count) = ")
(S2 'reset-count)

(display "(S2 5) = ")
(S2 5)

(display "(S2 how-many-calls?) = ")
(S2 'how-many-calls?)

(display "(S1 7) = ")
(S1 7)

(display "(S1 how-many-calls?) = ")
(S1 'how-many-calls?)




(newline)
