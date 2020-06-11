#lang sicp

;; ------------- new abstraction ----------------
(define (scan-current-frame var env succeed fail)
  (define (scan vars vals)
    (cond ((null? vars)
           (fail var env))
          ((eq? var (car vars)) (succeed vals))
          (else (scan (cdr vars) (cdr vals)))))
  (let ((frame (first-frame env)))
    (scan (frame-variables frame) (frame-values frame))))

(define (scan-environment var env succeed)
  (define (fail var env) (scan-environment var (enclosing-environment env) succeed))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan-current-frame var
                          env
                          succeed
                          fail)))


;; ------------ procedure updates ---------------
(define (lookup-variable-value var env)
  (scan-environment var env car))

(define (set-variable-value! var val env)
  (scan-environment var env (lambda (vals) (set-car! vals val))))

(define (define-variable! var val env)
  (define (succeed vals) (set-car! vals val))
  (define (fail var env) (add-binding-to-frame! var val (first-frame env)))
  (scan-current-frame var env succeed fail))
  

;; ----------- unchanged from text --------------
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (make-frame variables values)
  (cons variables values))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


#| tests
(define env (extend-environment '(true false) (list #t #f) the-empty-environment))
(set! env (extend-environment '(x y z) '(1 2 3) env))
(set! env (extend-environment '(a b c) '((1 2 3) "hello" "world") env))

#| lookup
(lookup-variable-value 'false env)
(lookup-variable-value 'y env)
(lookup-variable-value 'a env)
;; (lookup-variable-value 'missing env)
;|#

#| set!
(set-variable-value! 'false #t env)
(set-variable-value! 'y '(42 420) env)
(set-variable-value! 'a "Say" env)
;; (set-variable-value! 'missing 69 env)

(lookup-variable-value 'false env)
(lookup-variable-value 'y env)
(lookup-variable-value 'a env)
;|#

;#| define
(define-variable! 'false #t env)
(define-variable! 'y '(42 420) env)
(define-variable! 'a "Say" env)
(define-variable! 'missing 69 env)

(lookup-variable-value 'false env)
(lookup-variable-value 'y env)
(lookup-variable-value 'a env)
(lookup-variable-value 'missing env)
;|#
;|#
