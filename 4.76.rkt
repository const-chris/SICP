#lang sicp
(#%require (file "./microshaft-db.rkt"))

;; This problem's strategy won't work with lisp-value or not, since those forms need all their variables bound before they can be evaluated

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge (qeval (first-conjunct conjuncts) frame-stream)
             (conjoin (rest-conjuncts conjuncts) frame-stream))))


(define (merge frame-stream-1 frame-stream-2)
  (stream-flatmap
    (lambda (pair-of-frames) (apply unify-frames pair-of-frames))
    (stream-cartesian-product frame-stream-1 frame-stream-2)))


(define (unify-frames f1 f2)
  (if (empty-frame? f1)
      (singleton-stream f2)
      (let* ((b1 (first-binding f1))
             (extension (extend-if-possible (binding-variable b1) (binding-value b1) f2)))
        (if (eq? 'failed extension)
            the-empty-stream
            (unify-frames (rest-bindings f1) extension)))))


(define (stream-cartesian-product xs ys)
  (stream-flatmap
    (lambda (x)
      (stream-map
        (lambda (y)
          (list x y))
        ys))
    xs))


(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))
(define (empty-frame? frame) (null? frame))


(put 'and 'qeval conjoin)

;#| tests
(run-query
  '(and (supervisor ?x (Bitdiddle Ben))
        (address ?x ?y)))
;|#
