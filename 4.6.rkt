#lang sicp
(#%require (file "./4.3.rkt"))
(#%provide (all-from (file "./4.3.rkt"))
           (all-defined))


(define (install-let-package)
  (display "installing 'let package... ")
  (define (let->combination exp)
    (cons (make-lambda (let-vars exp) (let-body exp))
          (let-vals exp)))
  (define (let-vars exp) (map car (let-declarations exp)))
  (define (let-vals exp) (map cadr (let-declarations exp)))
  (define (let-declarations exp) (cadr exp))
  (define (let-body exp) (cddr exp))  
  (put 'eval 'let (lambda (exp env) (eval (let->combination exp) env))))

(install-let-package)

#|; ------------ without data-directed eval ------------
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ...
        ((let? exp) (eval (let->combination exp) env))
        ...))

(define (let? exp) (tagged-list? exp 'let))
;|#


#| --------------------- test -----------------------
(define exp '(let ((x 2)) x))
(define eval-let (get 'eval 'let))

(eval-let exp the-global-environment) 
;|#
 