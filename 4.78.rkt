#lang sicp
(#%require (file "./utils/base.rkt"))
(#%require (file "./4.51.rkt"))

#|
One subtle difference between this version and the stream version is in the renaming of variables.
The number gets reset when we backtrack, so in the execution of (last-pair ?x (3)) from 4.62, for example:

Original output:
(last-pair (3) (3))
(last-pair (?head-2 3) (3))
(last-pair (?head-2 ?head-4 3) (3))
(last-pair (?head-2 ?head-4 ?head-6 3) (3))
(last-pair (?head-2 ?head-4 ?head-6 ?head-8 3) (3))
...

Amb-evaluator output:
(last-pair (3) (3))
(last-pair (?head-1 3) (3))
(last-pair (?head-1 ?head-2 3) (3))
(last-pair (?head-1 ?head-2 ?head-3 3) (3))
(last-pair (?head-1 ?head-2 ?head-3 ?head-4 3) (3))
...
|#

;; Input definitions
(define (input-definition expr)
  (ambeval expr
           the-global-environment
           (lambda (x y) 'ok)
           (lambda () 'oops)))

(define (input-definitions exps)
  (for-each input-definition exps))

;; Driver loop
(define (query-driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (let ((q (query-syntax-process input)))
            (if (assertion-to-be-added? q)
                (begin
                  (user-print
                   (ambeval
                     `(add-rule-or-assertion! (add-assertion-body (quote ,q)))
                     the-global-environment
                     (const "\nAssertion added to data base.")
                     (const "\nERROR: couldn't add assertion to data base.")))
                  (query-driver-loop))
                (ambeval
                  `(instantiate (quote ,q)
                                (qeval (quote ,q) '())
                                (lambda (v f) (contract-question-mark v)))
                  the-global-environment
                  (lambda (val next-alternative)
                    (announce-output output-prompt)
                    (user-print val)
                    (internal-loop next-alternative))
                  (lambda ()
                    (announce-output ";;; There are no more values of ")
                    (user-print input)
                    (query-driver-loop))))))))
  (internal-loop
    (lambda ()
      (announce-output ";;; There is no current problem")
      (query-driver-loop))))


(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
                (substring chars 1 (string-length chars))))
        symbol)))


;; Logic Programming Package
(define (install-query-evaluator-package)
  (input-definitions
    '(
      ;; Instatiation
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
        (copy exp))

      ;; The Evaluator
      (define (qeval query frame)
        (let ((qproc (get (type query) 'qeval)))
          (if qproc
              (qproc (contents query) frame)
              (simple-query query frame))))

      (define (simple-query query-pattern frame)
        (amb (find-assertions query-pattern frame)
             (apply-rules query-pattern frame)))

      (define (conjoin conjuncts frame)
        (if (empty-conjunction? conjuncts)
            frame
            (conjoin (rest-conjuncts conjuncts)
                     (qeval (first-conjunct conjuncts) frame))))

      (define (disjoin disjuncts frame)
        (require (not (empty-disjunction? disjuncts)))
        (amb (qeval (first-disjunct disjuncts) frame)
             (disjoin (rest-disjuncts disjuncts) frame)))

      (define (negate operands frame)
        (let ((found-result? false))
          (amb (begin
                 (qeval (negated-query operands) frame)
                 (permanent-set! found-result? true))
               'continue)
          (require (not found-result?))
          frame))

      (define (lisp-value call frame)
        (require (execute
                  (instantiate
                    call
                    frame
                    (lambda (v f)
                      (error "Unknown pat var: LISP-VALUE" v)))))
        frame)

      (define (execute expr)
        (apply-in-underlying-scheme (eval-in-underlying-scheme (predicate expr) (base-env))
                                    (args expr)))

      (define (always-true ignore frame) frame)

      ;; Finding Assertions by Pattern Matching
      (define (find-assertions pattern frame)
        (amb-map (lambda (assertion) (pattern-match pattern assertion frame))
                 (fetch-assertions pattern frame)))

      (define (pattern-match pat dat frame)
        (cond ((equal? pat dat) frame)
              ((var? pat) (extend-if-consistent pat dat frame))
              (else
                (if (pair? pat)
                    (if (pair? dat)
                        (pattern-match (cdr pat)
                                       (cdr dat)
                                       (pattern-match (car pat) (car dat) frame)))
                    (amb)))))

      (define (extend-if-consistent var dat frame)
        (let ((binding (binding-in-frame var frame)))
          (if binding
              (pattern-match (binding-value binding) dat frame)
              (extend var dat frame))))

      ;; Rules and Unification
      (define (apply-rules pattern frame)
        (amb-map (lambda (rule) (apply-a-rule rule pattern frame))
                 (fetch-rules pattern frame)))

      (define (apply-a-rule rule query-pattern query-frame)
        (let ((clean-rule (rename-variables-in rule)))
          (let ((unify-result (unify-match query-pattern
                                           (conclusion clean-rule)
                                           query-frame)))
            (qeval (rule-body clean-rule) unify-result))))

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
        (cond ((equal? p1 p2) frame)
              ((var? p1) (extend-if-possible p1 p2 frame))
              ((var? p2) (extend-if-possible p2 p1 frame))
              (else
                (if (pair? p1)
                    (if (pair? p2)
                        (unify-match (cdr p1)
                                     (cdr p2)
                                     (unify-match (car p1) (car p2) frame)))
                    (amb)))))

      (define (extend-if-possible var val frame)
        (let ((binding (binding-in-frame var frame)))
          (cond (binding (unify-match (binding-value binding) val frame))
                ((var? val)
                 (let ((binding (binding-in-frame val frame)))
                   (if binding
                       (unify-match
                         var (binding-value binding) frame)
                       (extend var val frame))))
                ((depends-on? val var frame)
                 (amb))  ;; changed
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
                 (if (tree-walk (car e))
                     true
                     (tree-walk (cdr e))))
                (else false)))
        (tree-walk exp))

      ;; Maintaining the Data Base
      (define THE-ASSERTIONS '())

      (define (fetch-assertions pattern frame)
        (if (use-index? pattern)
            (get-indexed-assertions pattern)
            (get-all-assertions)))

      (define (get-all-assertions) THE-ASSERTIONS)

      (define (get-indexed-assertions pattern)
        (get-list (index-key-of pattern) 'assertion-stream))

      (define (get-list key1 key2)
        (let ((s (get key1 key2)))
          (if s s '())))

      (define (use-index? pat) (constant-symbol? (car pat)))

      (define (add-rule-or-assertion! assertion)
        (add-assertion! assertion))

      (define (add-assertion! assertion)
        (store-assertion-in-index assertion)
        (let ((old-assertions THE-ASSERTIONS))
          (set! THE-ASSERTIONS
            (cons assertion old-assertions))
          'ok))

      (define (store-assertion-in-index assertion)
        (if (indexable? assertion)
            (let ((key (index-key-of assertion)))
              (let ((current-assertion-stream
                      (get-list key 'assertion-stream)))
                (put key
                     'assertion-stream
                     (cons
                       assertion
                       current-assertion-stream))))))

      (define THE-RULES '())

      (define (fetch-rules pattern frame)
        (if (use-index? pattern)
            (get-indexed-rules pattern)
            (get-all-rules)))

      (define (get-all-rules) THE-RULES)

      (define (get-indexed-rules pattern)
        (append
          (get-list (index-key-of pattern) 'rule-stream)
          (get-list '? 'rule-stream)))

      (define (add-rule-or-assertion! assertion)
        (if (rule? assertion)
            (add-rule! assertion)
            (add-assertion! assertion)))

      (define (add-rule! rule)
        (store-rule-in-index rule)
        (let ((old-rules THE-RULES))
          (set! THE-RULES (cons rule old-rules))
          'ok))

      (define (store-rule-in-index rule)
        (let ((pattern (conclusion rule)))
          (if (indexable? pattern)
              (let ((key (index-key-of pattern)))
                (let ((current-rules (get-list key 'rule-stream)))
                  (put key 'rule-stream (cons rule current-rules)))))))

      (define (indexable? pat)
        (if (constant-symbol? (car pat))
            true
            (var? (car pat))))

      (define (index-key-of pat)
        (let ((key (car pat)))
          (if (var? key) '? key)))

      (define (use-index? pat) (constant-symbol? (car pat)))

      ;; Amb-map
      (define (amb-map f xs)
        (if (null? xs)
            (amb)
            (amb (f (car xs))
                 (amb-map f (cdr xs)))))

      ;; Query Syntax Procedures
      (define (tagged-list? exp tag)
        (if (pair? exp)
            (eq? (car exp) tag)
            false))

      (define (type exp)
        (if (pair? exp)
            (car exp)
            (error "Unknown expression TYPE" exp)))

      (define (contents exp)
        (if (pair? exp)
            (cdr exp)
            (error "Unknown expression CONTENTS" exp)))

      (define (add-assertion-body exp) (car (contents exp)))

      (define (empty-conjunction? exps) (null? exps))
      (define (first-conjunct exps) (car exps))
      (define (rest-conjuncts exps) (cdr exps))
      (define (empty-disjunction? exps) (null? exps))
      (define (first-disjunct exps) (car exps))
      (define (rest-disjuncts exps) (cdr exps))
      (define (negated-query exps) (car exps))
      (define (predicate exps) (car exps))
      (define (args exps) (cdr exps))

      (define (rule? statement) (tagged-list? statement 'rule))

      (define (conclusion rule) (cadr rule))

      (define (rule-body rule)
        (if (null? (cddr rule)) '(always-true) (caddr rule)))

      (define (var? exp) (tagged-list? exp '?))

      (define (constant-symbol? exp) (symbol? exp))

      (define rule-counter 0)

      (define (new-rule-application-id)
        (set! rule-counter (+ 1 rule-counter))
        rule-counter)

      (define (make-new-variable var rule-application-id)
        (cons '? (cons rule-application-id (cdr var))))

      (define (contract-question-mark variable)
        (string->symbol
          (string-append "?"
                         (if (number? (cadr variable))
                             (string-append (symbol->string (caddr variable))
                                            "-"
                                            (number->string (cadr variable)))
                             (symbol->string (cadr variable))))))

      ;; Frames and Bindings
      (define (make-binding variable value)
        (cons variable value))

      (define (binding-variable binding) (car binding))

      (define (binding-value binding) (cdr binding))

      (define (binding-in-frame variable frame)
        (assoc variable frame))

      (define (extend variable value frame)
        (cons (make-binding variable value) frame))

      ;; Data-directed Table
      (define (make-table)
        (let ((local-table (list '*table*)))
          (define (lookup key-1 key-2)
            (let ((subtable
                    (assoc key-1 (cdr local-table))))
              (if subtable
                  (let ((record
                          (assoc key-2 (cdr subtable))))
                    (if record (cdr record) false))
                  false)))
          (define (insert! key-1 key-2 value)
            (let ((subtable
                    (assoc key-1 (cdr local-table))))
              (if subtable
                  (let ((record
                          (assoc key-2 (cdr subtable))))
                    (if record
                        (set-cdr! record value)
                        (set-cdr! subtable
                                  (cons (cons key-2 value)
                                        (cdr subtable)))))
                  (set-cdr! local-table
                            (cons (list key-1 (cons key-2 value))
                                  (cdr local-table)))))
            'ok)
          (define (dispatch m)
            (cond ((eq? m 'lookup) lookup)
                  ((eq? m 'insert!) insert!)
                  (else (error "Unknown operation: TABLE" m))))
          dispatch))

      (define table (make-table))
      (define get (table 'lookup))
      (define put (table 'insert!))

      ;; Install QProc Package
      (put 'and 'qeval conjoin)
      (put 'or 'qeval disjoin)
      (put 'not 'qeval negate)
      (put 'lisp-value 'qeval lisp-value)
      (put 'always-true 'qeval always-true)

      ;; Initialize microshaft data base
      (add-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
      (add-assertion! '(job (Bitdiddle Ben) (computer wizard)))
      (add-assertion! '(salary (Bitdiddle Ben) 60000))
      (add-assertion! '(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
      (add-assertion! '(job (Hacker Alyssa P) (computer programmer)))
      (add-assertion! '(salary (Hacker Alyssa P) 40000))
      (add-assertion! '(supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
      (add-assertion! '(address (Fect Cy D) (Cambridge (Ames Street) 3)))
      (add-assertion! '(job (Fect Cy D) (computer programmer)))
      (add-assertion! '(salary (Fect Cy D) 35000))
      (add-assertion! '(supervisor (Fect Cy D) (Bitdiddle Ben)))
      (add-assertion! '(address (Tweakit Lem E) (Boston (Bay State Road) 22)))
      (add-assertion! '(job (Tweakit Lem E) (computer technician)))
      (add-assertion! '(salary (Tweakit Lem E) 25000))
      (add-assertion! '(supervisor (Tweakit Lem E) (Bitdiddle Ben)))
      (add-assertion! '(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
      (add-assertion! '(job (Reasoner Louis) (computer programmer trainee)))
      (add-assertion! '(salary (Reasoner Louis) 30000))
      (add-assertion! '(supervisor (Reasoner Louis) (Hacker Alyssa P)))
      (add-assertion! '(supervisor (Bitdiddle Ben) (Warbucks Oliver)))
      (add-assertion! '(address (Warbucks Oliver) (Swellesley (Top Heap Road))))
      (add-assertion! '(job (Warbucks Oliver) (administration big wheel)))
      (add-assertion! '(salary (Warbucks Oliver) 150000))
      (add-assertion! '(address (Scrooge Eben) (Weston (Shady Lane) 10)))
      (add-assertion! '(job (Scrooge Eben) (accounting chief accountant)))
      (add-assertion! '(salary (Scrooge Eben) 75000))
      (add-assertion! '(supervisor (Scrooge Eben) (Warbucks Oliver)))
      (add-assertion! '(address (Cratchet Robert) (Allston (N Harvard Street) 16)))
      (add-assertion! '(job (Cratchet Robert) (accounting scrivener)))
      (add-assertion! '(salary (Cratchet Robert) 18000))
      (add-assertion! '(supervisor (Cratchet Robert) (Scrooge Eben)))
      (add-assertion! '(address (Aull DeWitt) (Slumerville (Onion Square) 5)))
      (add-assertion! '(job (Aull DeWitt) (administration secretary)))
      (add-assertion! '(salary (Aull DeWitt) 25000))
      (add-assertion! '(supervisor (Aull DeWitt) (Warbucks Oliver)))
      (add-assertion! '(can-do-job (computer wizard) (computer programmer)))
      (add-assertion! '(can-do-job (computer wizard) (computer technician)))
      (add-assertion! '(can-do-job (computer programmer) (computer programmer trainee)))
      (add-assertion! '(can-do-job (administration secretary) (administration big wheel))))))

(install-query-evaluator-package)

(query-driver-loop)
