#lang sicp
(#%require (file "./4.3.rkt"))
(#%require (file "./4.25.rkt"))

#|
Both Ben and Alyssa are correct.

To implement unless under applicative-order evaluation, as Ben says, you'd need to install it as a special form.
Something like:
|#
(define (install-unless-package)
  (define (unless-predicate exp) (cadr exp))

  (define (unless-consequent exp) (caddr exp))

  (define (unless-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

  (define (unless->if exp)
    (make-if (unless-predicate exp)
             (unless-alternative exp)
             (unless-consequent exp)))

  (display "installing unless package... ")
  (put 'eval 'unless (lambda (exp env) (eval (unless->if exp) env))))

(install-unless-package)

#| tests
(define exp1 '(unless (= 0 1) 'correct 'huh?))
(define exp2 '(unless (= 0 1) 'correct))
(define exp3 '(unless (= 1 1) 'huh? 'correct))
(define exp4 '(unless (= 1 1) 'huh?))

(eval exp1 the-global-environment)
(eval exp2 the-global-environment)
(eval exp3 the-global-environment)
(eval exp4 the-global-environment)
;|#

#|
Alyssa is correct, however, in saying that with this implementation, unless is pure syntax and not a procedure.
If it were a procedure, you'd be able to use it anywhere you can use a procedure; for example:
|#
(define prog '((define (map op xs)
                 (if (null? xs)
                     '()
                     (cons (op (car xs)) (map op (cdr xs)))))

               (define (unless condition usual-value exceptional-value)
                 (if condition exceptional-value usual-value))

               (map (lambda (xs) (apply unless xs)) '((#f yay oops) (#t doh wow)))))

(lazy-eval-sequence prog)
