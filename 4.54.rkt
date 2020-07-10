#lang sicp
(#%require (file "./data-directed-amb-evaluator.rkt"))


(define (analyze-require expr)
  (let ((pproc (analyze (require-predicate expr))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

(define (require-predicate expr) (cadr expr))


(define (install-amb-analyze-require-package)
  (display "installing amb-analyze-require-package... ")
  (put 'amb-analyze 'require analyze-require))

(install-amb-analyze-require-package)




#| test

(input-definition
  '(define (an-element-of items)
     (require (not (null? items)))
     (amb (car items) (an-element-of (cdr items)))))

(driver-loop)

;|#
