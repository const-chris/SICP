#lang sicp
(newline)

(define (average . xs)
  (/ (apply + xs) (length xs)))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (center x)
  (average (lower-bound x) (upper-bound x)))

(define (percent x)
  (let ((c (center x)))
    (/ (- (upper-bound x) c) c)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mult-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
      (error "division by interval spanning zero:" y)
      (mult-interval x
                     (make-interval (/ 1 (upper-bound y))
                                    (/ 1 (lower-bound y))))))




(define (par1 r1 r2)
  (div-interval (mult-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1) (div-interval one r2)))))



(define A (make-interval 99.0 101.0))
(define B (make-interval 98.0 102.0))

(define par1AB (par1 A B))
(define par2AB (par2 A B))

(display "(par1 A B) = ")
par1AB
(display "(par2 A B) = ")
par2AB

(display "\nCenter-percent form:\n")
(display (string-append "(par1 A B) = (" (number->string (center par1AB)) ", " (number->string (percent par1AB)) ")\n"))
(display (string-append "(par2 A B) = (" (number->string (center par2AB)) ", " (number->string (percent par2AB)) ")\n"))



(newline)

