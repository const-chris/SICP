#lang racket
(require "2.64.rkt") ; list->tree

(define (range n)
  (define (iter current result)
    (if (= current 0)
        result
        (iter (- current 1) (cons current result))))
  (iter n '()))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))




(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (entry set-of-records)) true)
        ((< given-key (entry set-of-records)) (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))




(define test-records (list->tree '(1 2 5 6 8 10 13 17)))
(display "set-of-records: ")
test-records
(newline)




(define end-range 20)

(display "\nFor keys in [1, ")
(display end-range)
(display "]:\n")
(display "\nkey      found?\n")
(display "---------------\n")
(for-each (lambda (key)
            (display key)
            (display (if (< key 10) " " ""))
            (display "       ")
            (display (lookup key test-records))
            (newline))
          (range end-range))




(newline)
