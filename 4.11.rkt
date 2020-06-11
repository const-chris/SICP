#lang sicp
(#%require (file "utils/zip.rkt"))

;; ---------- unchanged from text -------------
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
;; --------------------------------------------

(define (make-frame variables values) (zip variables values))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cadr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (list var val)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
            ((eq? var (caar frame)) (cadar frame))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
            ((eq? var (caar frame)) (set-car! frame (list var val)))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((the-frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame) (add-binding-to-frame! var val the-frame))
            ((eq? var (caar frame)) (set-car! frame (list var val)))
            (else (scan (cdr frame)))))
    (scan the-frame)))


#| tests
;; (define test (make-frame '(x y z) '(1 2 3)))
;; (add-binding-to-frame! 'w 0 test)
;; test

;; (frame-variables test)
;; (frame-values test)

(define base-1 (extend-environment '(true false) '(1 0) the-empty-environment))
 base-1

(define base-2 (extend-environment '(x y z) '(1 2 3) base-1))
;; base-2

(define env (extend-environment '(a b c) '("hello" "goodbye" (1 2 3)) base-2))
;; env
;; (enclosing-environment env)
;; (lookup-variable-value 'a env)
;; (lookup-variable-value 'true env)
;; (lookup-variable-value 'missing env)

(set-variable-value! 'a 42 env)
;; (lookup-variable-value 'a env)

(define-variable! 'e 5 env)
(define-variable! 'a 42 env)
(define-variable! 'true 69 env)
;; (lookup-variable-value 'e env)
;; (lookup-variable-value 'a env)
;; (lookup-variable-value 'true env)
;; env
;|#
