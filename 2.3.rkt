#lang sicp
(newline)

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))




; rectangle implementation 1
(define (make-rect-1 a b)
  (cons a b))

(define (width-rect-1 r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

(define (height-rect-1 r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))


(define r (make-rect-1 (make-point 0 0) (make-point 3 4)))




; rectangle implementation 2
(define (make-rect-2 a w h)
  (cons a (cons w h)))

(define (width-rect-2 r) (cadr r))

(define (height-rect-2 r) (cddr r))


(define r-2 (make-rect-2 (make-point 0 0) 3 4))




; procedures that use rectangles
(define (test-implementation x r)
  (let ((width-rect (if (= x 1)
                        width-rect-1
                        width-rect-2))
        (height-rect (if (= x 1)
                         height-rect-1
                         height-rect-2)))
        
    (define (perimeter-rect r)
      (* 2 (+ (width-rect r) (height-rect r))))

    (define (area-rect r)
      (* (width-rect r) (height-rect r)))

    (newline)
    (display "perimeter = ")
    (display (perimeter-rect r))
    (newline)
    (display "area = ")
    (display (area-rect r))
    (newline)))




(display "testing implementation 1:")
(test-implementation 1 r)

(display "\ntesting implementation 2:")
(test-implementation 2 r-2)




(newline)





