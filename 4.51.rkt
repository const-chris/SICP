#lang sicp
(#%require (file "./4.39.rkt"))
#| (#%provide (all-defined)) |#

(define (install-amb-analyze-permanent-set!-package)
  (display "installing amb-analyze-permanent-set!-package... ")

  (define (analyze-permanent-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
      (lambda (env succeed fail)
        (vproc env
               (lambda (val fail2)
                 (set-variable-value! var val env)
                 (succeed 'ok fail2))
               fail))))

  (put 'amb-analyze 'permanent-set! analyze-permanent-assignment))



(install-amb-analyze-permanent-set!-package)

#| test
(input-definition
  '(define count 0))

(input-definition
  '(define (test)
     (let ((x (an-element-of '(a b c)))
           (y (an-element-of '(a b c))))
       (permanent-set! count (+ count 1))
       (require (not (eq? x y)))
       (list x y count))))


;; If we use set! instead of permanent-set!, count* = 1 + the value of count* at the time of calling (test*), no matter how many times we try-again.

(input-definition
  '(define count* 0))

(input-definition
  '(define (test*)
     (let ((x (an-element-of '(a b c)))
           (y (an-element-of '(a b c))))
       (set! count* (+ count* 1))
       (require (not (eq? x y)))
       (list x y count*))))


(driver-loop)
;|#
