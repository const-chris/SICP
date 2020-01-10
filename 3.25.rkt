#lang sicp

(define (assoc key records)
  (cond ((not (pair? records)) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))




(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter keys subtable)
        (let ((record (assoc (car keys) (cdr subtable))))
          (cond ((not record) false)
                ((null? (cdr keys)) (cdr record))
                (else
                 (iter (cdr keys) record)))))
      (iter keys local-table))
  
    (define (insert! keys value)
      (define (iter keys subtable)
        (let ((record (assoc (car keys) (cdr subtable))))
          (cond ((and (not record) (null? (cdr keys)))
                 (set-cdr! subtable
                           (cons (cons (car keys) value)
                                 (cdr subtable))))
                ((null? (cdr keys))
                 (set-cdr! record value))
                ((not record)
                 (set-cdr! subtable
                           (cons (list (car keys))
                                 (cdr subtable)))
                 (iter (cdr keys) (cadr subtable)))
                ((not (pair? (cdr record)))
                 (set-cdr! record '())
                 (iter (cdr keys) record))
                (else
                 (iter (cdr keys) record)))))              
      (iter keys local-table)
      'ok)

    (define (show) (cdr local-table))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'show-proc) show)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))




      

;; tests
(newline)

(define t (make-table))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))
(define show (t' show-proc))

(define (pp table)
  (define (indent indentation)
    (if (= indentation 0)
        ""
        (string-append "  " (indent (- indentation 1)))))

  (define (iter table indentation)
    (cond ((null? table) (string-append "\n" (indent (- indentation 1)) "}"))
          ((not (pair? table)) (string-append (number->string table)))
          ((pair? (car table))
           (string-append (iter (car table) indentation)
                          (iter (cdr table) indentation)))
          (else
           (string-append "\n"
                          (indent indentation)
                          (symbol->string (car table))
                          (if (and (pair? (cdr table)) (pair? (cadr table)))
                              (string-append ": {" (iter (cdr table) (+ indentation 1)))
                              (string-append ": " (iter (cdr table) indentation)))))))
  (display "{")
  (display (iter table 1))
  (newline))





(put '(a b) 1)
(put '(a c) 2)
(get '(a b))
(get '(a c))
(put '(a b) 3)
(get '(a b))
(put '(a c d e) 4)
(show)
(pp (show))
(get '(a c))
(get '(a c d))
(get '(a))
(get '(a c d e))
(put '(a c) 2)
(get '(a c))
(get '(a c d e))

(show)
(pp (show))

(newline)