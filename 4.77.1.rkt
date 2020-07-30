#lang sicp
(#%require (file "./utils/filter.rkt"))
(#%require (file "./utils/eval-in-underlying-scheme.rkt"))
(#%require (file "./microshaft-db.rkt"))
(#%provide (all-from (file "./microshaft-db.rkt"))
 (all-defined))
;; REVISIT -- implement lisp-value

#|
We want filtering (using not and lisp-value) to be delayed until all the variable dependencies of the filters have been bound.

We can attach a procedure to the frame, which checks dependencies and calls a filter if appropriate.
Then we can have extend check for the presence of such a procedure (it could be tagged) and after extending the frame, call it.
This seems simpler.
|#

;; new implementation of extend
(define (extend variable value frame)
  (let ((new-binding (make-binding variable value)))
    (if (awaiting-filter? frame)
        ((pending-filter frame) (cons new-binding (bindings frame)))
        (cons new-binding frame))))

(define (awaiting-filter? frame)
  (and (not (null? frame))
       (tagged-list? (car frame) 'awaiting-filter)))

(define (pending-filter frame) (cadr (car frame)))
(define (last-ditch-filter frame) (caddr (car frame)))
(define (bindings frame) (cdr frame))


;; wrapper for instantiate, called by query-driver-loop and run-query
(define (excecute-pending-and-instantiate expr frame unbound-var-handler)
  (let ((frame (if (awaiting-filter? frame)
                   ((last-ditch-filter frame) (bindings frame))
                   frame)))
    (instantiate expr frame unbound-var-handler)))


;; new implementation of negate
(define (negate operands frame-stream)
  (let ((deps (extract-deps operands)))
    (stream-flatmap
      (lambda (frame)
        (let ((result-frame (negate-in-frame operands frame deps)))
          (if (eq? result-frame 'failed)
              the-empty-stream
              (singleton-stream result-frame))))
      frame-stream)))

(define (negate-in-frame operands frame deps)
  (let* ((next-deps (filter (unbound-in? frame) deps)))
    (if (null? next-deps)
        (if (stream-null?
              (qeval (negated-query operands)
                     (singleton-stream frame)))
            frame
            'failed)
        (cons (list
                'awaiting-filter
                (make-pending-negate operands next-deps)
                (make-last-ditch-negate operands))
              frame))))

(define (make-pending-negate operands deps)
  (lambda (frame)
    (negate-in-frame operands frame deps)))

(define (make-last-ditch-negate operands)
  (lambda (frame)
    (if (stream-null?
          (qeval (negated-query operands)
                 (singleton-stream frame)))
        frame
        'failed)))


;; TODO new implementation of lisp-value
(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate
              call
              frame
              (lambda (v f)
                (error "Unknown pat var: LISP-VALUE" v))))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))


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




;; 4.4.4.1 The Driver Loop and Instantiation
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
              (stream-flatmap                        ;; changed
                (lambda (frame)
                  (excecute-pending-and-instantiate  ;; changed
                    q
                    frame
                    (lambda (v f)
                      (contract-question-mark v))))
                (qeval q (singleton-stream '()))))))))

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
              (stream-flatmap                         ;; changed
                (lambda (frame)
                  (excecute-pending-and-instantiate   ;; changed
                    q
                    frame
                    (lambda (v f)
                      (contract-question-mark v))))
                (qeval q (singleton-stream '()))))
            (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (if (eq? frame 'failed)              ;; changed
      the-empty-stream                 ;; changed
      (singleton-stream (copy exp))))  ;; changed

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

;; cleaning up output
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

;; need to do the same kind of failsafe as 4.77.0 for queries like this to work
(run-query
  '(and (not (and (job ?x (computer ?type))
                  (job ?x (?division programmer))))
        (supervisor ?x ?y)))

;|#
