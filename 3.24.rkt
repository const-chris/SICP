#lang sicp

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
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
      (display "--- ")
      (display local-table)
      (newline)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))



;; tests
(newline)


(define (same-key? x k)
  (< (abs (- x k)) 1))
(display "
(define (same-key? x k)
  (< (abs (- x k)) 1))\n\n")

(define t (make-table same-key?))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))


(display "(put 0.1 0.5 'a) = ")
(put 0.1 0.5 'a)

(display "(put 2.3 1.7 'b) = ")
(put 2.3 1.7 'b)


(display "(get 1 1) = ")
(get 1 1)

(display "(get 2 2) = ")
(get 2 2)

(display "(get 3 3) = ")
(get 3 3)

(display "(get 3 2) = ")
(get 3 2)




(newline)
