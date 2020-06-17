#lang sicp

;; a)
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;; naive solution
(define fib
  (lambda (n)
    ((lambda (fib) (fib fib n))
     (lambda (f k)
       (cond ((= k 0) 0)
             ((= k 1) 1)
             (else (+ (f f (- k 1)) (f f (- k 2)))))))))

;; iterative solution
(define (fib-iter n)
  (if (= 0 n)
      0
      ((lambda (iter) (iter iter 0 1 1))
       (lambda (it prev curr i)
         (if (= i n)
             curr
             (it it curr (+ prev curr) (+ i 1)))))))

(map fib '(0 1 2 3 4 5 6 7 8 9))
(map fib-iter '(0 1 2 3 4 5 6 7 8 9))


;; b)
(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))

(map f '(0 1 2 3 4 5 6 7 8 9))
