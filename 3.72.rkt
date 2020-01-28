#lang sicp
(#%require (file "utils/square.rkt"))
(#%require (file "stream-utils.rkt"))
(#%require (file "3.70.rkt"))

(define (sum-squares i j)
  (+ (square i)
     (square j)))

(define sums-squares
  (stream-map (lambda (p) (cons (apply sum-squares p) p))
              (weighted-pairs integers integers sum-squares)))

(define inverted-ramanujan
  (stream-map (lambda (group)
                (cons (car (car group))
                      (map cdr group)))
              (stream-filter (lambda (group)
                               (= (car (car group))
                                  (car (cadr group))
                                  (car (caddr group))))
                             (stream-map list
                                         sums-squares
                                         (stream-cdr sums-squares)
                                         (stream-cdr (stream-cdr sums-squares))))))

(stream-take 3 inverted-ramanujan)




#|
oops, misread

(define (sum-cubes i j)
  (+ (* i i i)
     (* j j j)))

(define sums-cubes
  (stream-map (lambda (p) (cons (apply sum-cubes p) p))
              (weighted-pairs integers integers sum-cubes)))

(define groups-of-three
  (stream-map list
              sums-cubes
              (stream-cdr sums-cubes)
              (stream-cdr (stream-cdr sums-cubes))))

(define super-ramanujan
  (stream-map (lambda (group)
                (cons (car (car group))
                      (map cdr group)))
              (stream-filter (lambda (group)
                               (= (car (car group))
                                  (car (cadr group))
                                  (car (caddr group))))
                             groups-of-three)))

(stream-take 3 super-ramanujan)
;|#
