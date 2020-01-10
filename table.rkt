#lang racket
(provide make-table)

(define (map-idx proc items)
  (define (m idx items)
    (if (null? items)
        '()
        (cons (proc (car items) idx)
              (m (+ idx 1) (cdr items)))))
  (m 0 items))

(define (repeat times elem)
  (if (= times 0)
      '()
      (cons elem (repeat (- times 1) elem))))


(define (make-table)
  (let ((table '())
        (rows '())
        (cols '()))
    
    (define (find-row row)
      (define (iter idx rows)
        (cond ((null? rows) false)
              ((equal? (car rows) row) idx)
              (else
               (iter (+ idx 1) (cdr rows)))))
      (iter 0 rows))

    (define (find-col col)
      (define (iter idx cols)
        (cond ((null? cols) false)
              ((equal? (car cols) col) idx)
              (else
               (iter (+ idx 1) (cdr cols)))))
      (iter 0 cols))

    (define (get row col)
      (let ((row-idx (find-row row))
            (col-idx (find-col col)))
        (if (not (and row-idx col-idx))
            false
            (list-ref (list-ref table row-idx)
                      col-idx))))

    (define (put row col contents)
      (let ((row-idx (find-row row))
            (col-idx (find-col col)))                
        (cond ((and (not col-idx) (not row-idx))
               (set! rows (cons row rows))
               (set! cols (cons col cols))
               (let ((new-rows (map (lambda (x) (cons false x)) table)))
                 (set! table
                       (cons (cons contents (repeat (- (length cols) 1) false))
                             new-rows))))
              ((not col-idx)
               (set! cols (cons col cols))
               (set! table (map-idx (lambda (row i)
                                      (if (= i row-idx)
                                          (cons contents row)
                                          (cons false row)))
                                    table)))
              ((not row-idx)
               (set! rows (cons row rows))
               (set! table (cons (map-idx (lambda (_ i)
                                            (if (= i col-idx)
                                                contents
                                                false))
                                          cols)
                                 table)))
              (else
               (set! table (map-idx (lambda (row i)
                                      (if (not (= i row-idx))
                                          row
                                          (map-idx (lambda (val j)
                                                     (if (= j col-idx)
                                                         contents
                                                         val))
                                                   row)))
                                    table))))))
    ;#|
    (define (print)
      (display "\nrows: ")
      (display rows)
      (display "\ncols: ")
      (display cols)
      (newline)
      (display table))
    ;|#

    (values get put print)))

#|
(define-values (get put print) (make-table))
(put 'x 'y 3)
(put 'x 'z 4)
(put 'a 'z 5)
(put 'a 'z 0)
(get 'a 'z)
(print)
;(update-table identity)
;|#

