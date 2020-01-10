#lang sicp
(newline)

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start (enumerate-interval (+ start 1) end))))

(define (sum items)
  (accumulate + 0 items))

(define (make-sum-pair pair)
  (list (car pair) (cadr pair) (sum pair)))

(define (prime-sum? pair)
  (prime? (sum pair)))

(define (prime? n)
  (define (iter i)
    (cond ((> (* i i) n) true)
          ((= (remainder n i) 0) false)
          (else (iter (+ i 1)))))
  (iter 2))

(define (filter pred seq)
  (accumulate (lambda (x y)
                (if (pred x)
                    (cons x y)
                    y))
              nil
              seq))




(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))




; simplified definition of prime-sum-pairs
(define (prime-sum-pairs n)
  (map make-sum-pair
       (filter prime-sum?
               (unique-pairs n))))


(display "Implemented using unique-pairs,
(prime-sum-pairs 6) = ")
(prime-sum-pairs 6)




(newline)