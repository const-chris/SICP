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

(define (filter pred seq)
  (accumulate (lambda (x y)
                (if (pred x)
                    (cons x y)
                    y))
              nil
              seq))




(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))




(define (triples-with-sum s n)
  (filter (lambda (x) (= (sum x) s))
          (unique-triples n)))




(display "(triples-with-sum 12 9) = ")
(triples-with-sum 12 9)




(newline)