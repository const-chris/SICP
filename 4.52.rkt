#lang sicp
(#%require (file "./4.35.rkt"))
#| (#%provide (all-defined)) |#

(define (analyze-if-fail expr)
  (let ((tproc (analyze (if-fail-try expr)))
        (cproc (analyze (if-fail-catch expr))))
    (lambda (env succeed fail)
      (tproc env
             (lambda (try-val fail2)
               (succeed try-val fail2))
             (lambda ()
               (succeed (cproc env
                               (lambda (catch-val fail3) catch-val)
                               fail)
                        fail))))))

(define (if-fail-try expr) (cadr expr))
(define (if-fail-catch expr) (caddr expr))

(define (install-amb-analyze-if-fail-package)
  (display "installing amb-analyze-if-fail-package... ")
  (put 'amb-analyze 'if-fail analyze-if-fail))

(install-amb-analyze-if-fail-package)

#| (driver-loop) |#
