#lang scheme
(newline)

(define (split-at xs n)
  (define (step takes drops count)
    (if (< count n)
        (step (append takes (list (car drops))) (cdr drops) (+ count 1))  ; could be more efficient
        (values takes drops)))
  (step '() xs 0))


(define (sum_squares x y)
  (+ (* x x) (* y y)))


(define (lesser a b)
  (if (and (real? a) (real? b))
      (if (<= a b)
          a
          b)
      null))

(define (greater a b)
  (if (and (real? a) (real? b))
      (if (>= a b)
          a
          b)
      null))


(define (min . xs)
  (cond ((null? xs) null)
        ((null? (cdr xs))
         (if (real? (car xs))
             (car xs)
             null))
        (else (lesser (car xs) (apply min (cdr xs))))))

(define (max . xs)
  (cond ((null? xs) null)
        ((null? (cdr xs))
         (if (real? (car xs))
             (car xs)
             null))
        (else (greater (car xs) (apply max (cdr xs))))))




; 1.3
(define (f x y z)
  (let ((least (min x y z)))    
    (cond ((= x least) (sum_squares y z))
          ((= y least) (sum_squares x z))
          (else (sum_squares x y)))))

(display "(f 3 4 4): ")
(f 3 4 4)


(define (max-n n xs)
  (let-values (((first-n last-k-minus-n) (split-at xs n)))
    (define (check-next rest maxes)
      (if (null? rest)
          maxes
          (if (> (car rest) (car maxes))
              (check-next (cdr rest) (sort (cons (car rest) (cdr maxes)) <))
              (check-next (cdr rest) maxes))))
    (check-next last-k-minus-n (sort first-n <))))  ; more efficient would be to replace these two calls to sort with min-heapify
    
(display "(max-n 3 '(1 23 4 21 -90 19 83)): ")
(max-n 3 '(1 23 4 21 8 -90 19 83))


(define (g . xs)
  (apply sum_squares (max-n 2 xs)))

(display "(g 2 3 4 5): ")
(g 2 3 4 5)




; 1.4
(define (a-plus-abs-b a b)
  ((if (< b 0) - +) a b))




; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; (test 0 (p))
; runs forever, so this interpreter uses applicative-order evaluation


(newline)
(display "idea for next time: heap data structure to make max-n more efficient")
(newline)
(newline)