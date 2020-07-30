#lang sicp
(#%require (file "./utils/base.rkt"))
(#%require (file "./utils/filter.rkt"))
(#%require (file "./utils/eval-in-underlying-scheme.rkt"))
(#%require (file "./microshaft-db.rkt"))
(#%provide (all-from (file "./microshaft-db.rkt"))
 (all-defined))
;; REVISIT -- implement failsafe for lisp-value

#|
We want filtering (using not and lisp-value) to be delayed until all the variable dependencies of the filters have been bound.

First idea:
To accomplish this, we'll re-implement frames using message-passing.
When negate or lisp-value is called, it will extract the dependencies for the contents of the query (all the variables in the expression) into a list.
It will then check to see that all those variables are bound in the current frame.
If they are, it will proceed with filtering as normal.
If they aren't, it will return a new frame containing the same bindings as the frame it received, but which acts as a proxy and whenever a new variable is bound in the frame, if that variable is in the dependency list, removes it from the dependency list.
When the dependency list becomes empty, the proxy frame performs the filtering operation.
If a proxy still exists when we try to do the final instantiation of the query, we first execute the filtering operation delayed in the proxy.
|#

;; New implementation of frames (4.4.4.8)
(define (make-frame bindings)
  (lambda (m)
    (cond ((eq? m 'binding-in-frame)
           (lambda (variable)
             (assoc variable bindings)))
          ((eq? m 'extend)
           (lambda (variable value)
             (make-frame (cons (make-binding variable value) bindings))))
          ((eq? m 'instantiate)
           (lambda (expr frame unbound-var-handler)
             (define (copy expr)
               (cond ((var? expr)
                      (let ((binding (binding-in-frame expr frame)))
                        (if binding
                            (copy (binding-value binding))
                            (unbound-var-handler expr frame))))
                     ((pair? expr)
                      (cons (copy (car expr)) (copy (cdr expr))))
                     (else expr)))
             (singleton-stream (copy expr))))
           (else
             (error "invalid message -- MAKE-FRAME" m)))))

(define (binding-in-frame variable frame)
  ((frame 'binding-in-frame) variable))

(define (extend variable value frame)
  ((frame 'extend) variable value))

(define (instantiate expr frame unbound-var-handler)
  ((frame 'instantiate) expr frame unbound-var-handler))


;; New implementation of negate (4.4.4.2)
(define (negate operands frame-stream)
  (let ((deps (extract-deps operands)))
    (stream-flatmap
      (lambda (frame)
        (negate-in-frame
          operands
          frame
          deps
          singleton-stream
          the-empty-stream))
      frame-stream)))

(define (negate-in-frame operands frame deps succeed fail)
  (let* ((next-deps (filter (unbound-in? frame) deps))
        (proxy-extend (negate-extend operands frame next-deps)))
    (if (null? next-deps)
        (if (stream-null?
              (qeval (negated-query operands)
                     (singleton-stream frame)))
            (succeed frame)
            fail)
        (succeed (make-proxy-frame operands frame next-deps proxy-extend)))))

(define (negate-extend operands frame deps)
  (lambda (variable value)
    (negate-in-frame
      operands
      (extend variable value frame)
      deps
      id
      'failed)))


;; New implementation of lisp-value (4.4.4.2)
(define (lisp-value call frame-stream)
  (let ((deps (extract-deps call)))
    (stream-flatmap
      (lambda (frame)
        (lisp-value-in-frame
          call
          frame
          deps
          singleton-stream
          the-empty-stream))
      frame-stream)))

(define (lisp-value-in-frame call frame deps succeed fail)
  (let ((next-deps (filter (unbound-in? frame) deps))
        (proxy-extend (lisp-value-extend call frame deps)))
    (if (null? deps)
        (if (execute
              (instantiate
                call
                frame
                (lambda (v f)
                  (error "Unknown pat var: LISP-VALUE" v))))
            (succeed frame)
            fail)
        (succeed (make-proxy-frame call frame next-deps)))))

(define (lisp-value-extend call frame deps)
  (lambda (variable value)
    (lisp-value-in-frame
      call
      (extend variable value frame)
      deps
      id
      'failed)))


;; dependencies
(define (extract-deps query)
  (define (union vars deps)
    (cond ((null? vars) deps)
          ((member (car vars) deps) (union (cdr vars) deps))
          (else (cons (car vars) (union (cdr vars) deps)))))
  (define (tree-walk expr res)
    (cond ((var? expr) (union (list expr) res))
          ((pair? expr) (union (tree-walk (car expr) '())
                               (tree-walk (cdr expr) res)))
          (else res)))
  (tree-walk query '()))

(define (unbound-in? frame)
  (lambda (var)
    (not (binding-in-frame var frame))))


;; proxy frames
(define (make-proxy-frame query frame deps proxy-extend)
  (lambda (m)
    (cond ((eq? m 'binding-in-frame) (frame m))
          ((eq? m 'display) (frame m))
          ((eq? m 'extend) proxy-extend)
          ((eq? m 'instantiate)
           (if (stream-null?
                 (qeval (negated-query query)
                        (singleton-stream frame)))
               (frame m)
               (const the-empty-stream)))
          (else
            (error "invalid message -- MAKE-PROXY-FRAME" m)))))




(define (run-query query)
  (let ((q (query-syntax-process query)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base."))
          (else
            (newline)
            (display output-prompt)
            (display-stream
              (stream-flatmap   ;; changed
                (lambda (frame)
                  (instantiate
                    q
                    frame
                    (lambda (v f)
                      (contract-question-mark v))))
                (qeval q (singleton-stream (make-frame '())))))))))  ;; changed

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
            (newline)
            (display output-prompt)
            (display-stream
              (stream-flatmap   ;; changed
                (lambda (frame)
                  (instantiate
                    q
                    frame
                    (lambda (v f)
                      (contract-question-mark v))))
                (qeval q (singleton-stream (make-frame '())))))  ;; changed
            (query-driver-loop)))))

;; 4.4.4.2 The Evaluator
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay (apply-rules query-pattern frame))))
    frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
        (qeval (first-disjunct disjuncts) frame-stream)
        (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment
               (args exp))))

(define (always-true ignore frame-stream) frame-stream)

(define (install-qproc-package)
  (display "installing logic-evaluator qprocs... ")
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true))

(install-qproc-package)

;; 4.4.4.3 Finding Assertions by Pattern Matching
(define (find-assertions pattern frame)
  (stream-flatmap
    (lambda (datum) (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
          (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
           (cdr pat)
           (cdr dat)
           (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;; 4.4.4.4 Rules and Unification
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding (unify-match (binding-value binding) val frame))
          ((var? val) ; ***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                   var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) ; ***
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))


;#| tests

(run-query
  '(job ?x (?division programmer)))

(run-query
  '(and (supervisor ?x ?y)
        (not (job ?x (computer programmer)))))

(run-query
  '(and (not (job ?x (computer programmer)))
        (supervisor ?x ?y)))

(run-query
  '(not (baseball-fan (Bitdiddle Ben))))

(run-query
  '(and (not (and (job ?x (computer ?type))
                  (job ?x (?division programmer))))
        (supervisor ?x ?y)))

;|#
