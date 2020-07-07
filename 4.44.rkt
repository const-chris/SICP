#lang sicp
(#%require (file "./4.39.rkt"))

(define (queens)
  (define (diagonal-1 row col) (- row col))
  (define (diagonal-2 row col) (+ row col))
  (let ((res (map (lambda (x) (amb 1 2 3 4 5 6 7 8)) '(1 2 3 4 5 6 7 8))))
    (require (distinct? res))
    (require (distinct? (map-with-index diagonal-1 res)))
    (require (distinct? (map-with-index diagonal-2 res)))
    res))

(define (map-with-index proc xs)
  (define (map-iter xs i)
    (if (null? xs)
        '()
        (cons (proc (car xs) i)
              (map-iter (cdr xs) (+ i 1)))))
  (map-iter xs 0))

#| (queens) |#


#|
;; Could be sped up substantially at a significant cost to readability by using a nested let expression and weeding out failures early:

(define (queens*)
  (let* ((rows (list (amb 1 2 3 4 5 6 7 8)))
         (d1s (map-with-index - rows))
         (d2s (map-with-index + rows)))
    (define (loop col)
      (if (> col 8)
          rows
          (let* ((row (amb 1 2 3 4 5 6 7 8))
                 (d1 (- col row))
                 (d2 (+ col row)))
            (require (not (member row rows)))
            (require (not (member d1 d1s)))
            (require (not (member d2 d2s)))
            (set! rows (cons row rows))
            (set! d1s  (cons d1 d1s))
            (set! d2s  (cons d2 d2s))
            (loop (+ col 1)))))
    (loop 2)))

(queens*)
;|#


;#| repl
(input-definition '(define (map proc xs)
                     (if (null? xs)
                         '()
                         (cons (proc (car xs))
                               (map proc (cdr xs))))))

(input-definition '(define (map-with-index proc xs)
                     (define (map-iter xs i)
                       (if (null? xs)
                           '()
                           (cons (proc (car xs) i)
                                 (map-iter (cdr xs) (+ i 1)))))
                     (map-iter xs 0)))

(input-definition '(define (queens)
                     (define (diagonal-1 row col) (- row col))
                     (define (diagonal-2 row col) (+ row col))
                     (let ((res (map (lambda (x) (amb 1 2 3 4 5 6 7 8)) '(1 2 3 4 5 6 7 8))))
                       (require (distinct? res))
                       (require (distinct? (map-with-index diagonal-1 res)))
                       (require (distinct? (map-with-index diagonal-2 res)))
                       res)))

(input-definition '(define (queens*)
                     (let ((rows (list (amb 1 2 3 4 5 6 7 8))))
                       (let ((d1s (map-with-index - rows))
                             (d2s (map-with-index + rows)))
                         (define (loop col)
                           (if (> col 8)
                               rows
                               (let ((row (amb 1 2 3 4 5 6 7 8)))
                                 (let ((d1 (- col row))
                                       (d2 (+ col row)))
                                   (require (not (member row rows)))
                                   (require (not (member d1 d1s)))
                                   (require (not (member d2 d2s)))
                                   (set! rows (cons row rows))
                                   (set! d1s  (cons d1 d1s))
                                   (set! d2s  (cons d2 d2s))
                                   (loop (+ col 1))))))
                         (loop 2)))))


(driver-loop)
;|#

