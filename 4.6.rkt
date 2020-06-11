#lang sicp

;; from text
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

#| eval update and new predicate
(define (eval exp env)
  (cond
    ...
    ((let? exp) (eval (let->combination exp) env))
    ...
    ))


(define (let? exp) (tagged-list exp 'let))
|#


(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp))
        (let-vals exp)))


(define (let-vars exp) (map car (let-declarations exp)))
(define (let-vals exp) (map cadr (let-declarations exp)))

(define (let-declarations exp) (cadr exp))

(define (let-body exp) (cddr exp))


;#| test
(define exp '(let ((x 2)) (+ x 1)))

(let->combination exp)
;|#

   