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
          (user-print-list-literal input)
          (user-print output))))
  (driver-loop))

;#| 1)
(define (user-print-list-literal object)
  (display "(")
  (define (print-iter xs)
    (if (not (actual-value `(pair? ,xs) the-global-environment))
        (display (actual-value xs the-global-environment))
        (let ((next (list 'car xs)))
          (display (actual-value next the-global-environment))
          (display " ")
          (print-iter (list 'cdr xs)))))
  (print-iter object)
  (display ")"))
;|#

#| 2)
(define (user-print-list-literal object)
  (display "<lazy-list>"))
;|#
