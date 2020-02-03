#lang sicp
(#%require (file "utils/square.rkt"))
(#%require (file "constraints.rkt"))

;; Ben's squarer
(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? a)
           (set-value! b (square (get-value a)) me))
          ((has-value? b)
           (if (< (get-value b) 0)
               (error "square less than 0: SQUARER"
                      (get-value b))
               (set-value! a (sqrt (get-value b)) me)))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)


;#| test
(define a (make-connector))
(define b (make-connector))
(squarer a b)

(probe "x" a)
(probe "x^2" b)

(set-value! b 4 'user)
(forget-value! b 'user)

(set-value! a 3 'user)
(forget-value! a 'user)
;|#
