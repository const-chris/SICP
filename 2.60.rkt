#lang racket
(newline)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; O(n), Same as in the non-duplicate representation.
; Likely to be slower, however, as the sets will very likely be larger.


(define (adjoin-set x set) (cons x set))

; O(1) instead of O(n) for the non-duplicate representation.


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; O(n^2), Same as the non-duplicate representation.
; Like element-of-set, likely to be slower due to increased set size.
; Pay a higher price for the larger sets here, as this algorithm is O(n^2).
; Also note that this can still produce a set containing duplicates.
; Any element present in both sets will appear in the intersection as many times as it appears in set1.


(define (union-set set1 set2) (append set1 set2))

; O(n)--assuming append is O(n)--instead of O(n^2) for the non-duplicate representation.
; If append is optimized in the language, this could even be O(1).


; For applications where additions to sets (adjoins and unions) happen as or more often than
; selections (intersections and element-of-set? queries), this representation could be favorable.
; Most of the time, however, we probably want to inspect data at least as often as we want to add to it.