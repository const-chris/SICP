#lang sicp
(#%require (file "stream-utils.rkt"))
(#%require (file "3.77.rkt"))


(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (let ((iL '*unassigned)
          (vC '*unassigned)
          (diL '*unassigned)
          (dvC '*unassigned))
      (set! iL (integral (delay diL) iL0 dt))
      (set! vC (integral (delay dvC) vC0 dt))
      (set! diL (add-streams (scale-stream iL (- (/ R L)))
                             (scale-stream vC (/ 1 L))))
      (set! dvC (scale-stream iL (- (/ 1 C))))
      (stream-map cons vC iL))))


;; test
(define circuit (RLC 1 1 0.2 0.1))
(define model (circuit 10 0))
(stream-take 20 model)
