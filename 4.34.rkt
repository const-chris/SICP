#lang sicp
(#%require (file "./4.33.rkt"))

#|
There are two ways I can imagine to print a lazy-list:
1) display items one at a time, forcing them as we go
2) use a placeholder symbol
|#

;; first let's mark the procedure-body of a cons so the driver-loop can recognize it
(eval `(define (cons x y) (lambda (m) ,'cons (m x y))) the-global-environment)

(define (lazy-list? object)
  (and (compound-procedure? object)
       (tagged-list? (procedure-body object) 'cons)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (if (lazy-list? output)
          (user-print-list output)
          (user-print output))))
  (driver-loop))

;#| 1)
(define (exe expr) (actual-value expr the-global-environment))

(define (lift op object)
  (let ((procedure (actual-value op the-global-environment)))
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure (list object)))
          ((compound-procedure? procedure)
           (eval-sequence (procedure-body procedure)
                          (extend-environment (procedure-parameters procedure)
                                              (list object)
                                              (procedure-environment procedure))))
          (else (error "Unknown procedure type: LIFT" procedure)))))

(define (user-print-list object)
  (define (print-iter xs maybe-leading-space)
    (if (not (lift 'pair? xs))
        (if (not (lift 'null? xs))
            (begin
              (display " . ")
              (display xs)))
        (begin
          (display maybe-leading-space)
          (display (force-it (lift 'car xs)))
          (print-iter (force-it (lift 'cdr xs)) " "))))
  (display "(")
  (print-iter object "")
  (display ")"))
;|#

#| 2)
(define (user-print-list object)
  (display "<lazy-list>"))
;|#
