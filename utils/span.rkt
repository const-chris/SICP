#lang sicp
(#%provide span)

(define (span p xs)
  (if (null? xs)
      (cons '() '())
      (if (p (car xs))
          (let* ((res (span p (cdr xs)))
                 (ys (car res))
                 (zs (cdr res)))
            (cons (cons (car xs) ys) zs))
          (cons '() xs))))

#| test
(define xs '(1 2 3 4 5))
(define (p x) (> x 3))
(define (q x) (< x 3))
(define (r x) (< x 6))

(span p xs)
(span q xs)
(span r xs)
;|#
