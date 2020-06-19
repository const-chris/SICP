#lang sicp
(#%require (file "./evaluator-2.rkt"))

;; the text's
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

;; Alyssa's
(define (analyze-seq exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)))
          ((car procs) env)
          (else
            ((car procs) env)
            (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        ;; returns this lambda
        (lambda (env)
          (execute-sequence procs env)))))

#|
The key here is that Alyssa's analyze-sequence procedure returns an execution procedure of
the form

    (lambda (env)
      (execute-sequence (list proc-1 proc-2 ... proc-n) env))

Where procs are already analyzed, but the execute-sequence sequence call is a thunk.
Every time her execution procedure is called, that thunk will have to be evaluated, which
entails a call to execute-sequence, an O(n) operation where n is the length of the seqence.
In practice, the time complexity is probably less important than the constant-time overhead
of executing execute-sequence.

The version in the text does not have this problem. It builds up an execution procedure of
the form

    (lambda (env)
      (proc-1 env)
      (proc-2 env)
      ...
      (proc-n env))

Again, procs are already analyzed, and this time there's no intermediate procedure; the
sequence of procs *are* the body of the execution procedure. In fact, this execution procedure
is exactly the procedure returned by the application of execute-sequence inside Alyssa's
execution procedure, except it's generated at analysis-time rather than evaluation-time
(runtime).
|#
