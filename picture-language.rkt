#lang racket
(require "2.46.rkt" "2.47.rkt")
(provide draw-line rhombus window frame-coord-map transform-painter)


(require racket/gui/base)

(define picture-size 500)

(define bitmap
  (make-object bitmap% (+ picture-size 1) (+ picture-size 1)))

(define bitmap-dc
  (new bitmap-dc% [bitmap bitmap]))

(define frame
  (new frame% [label "SICP Picture Language"]))

(define canvas
  (new canvas%
       [parent frame]
       [min-width (+ picture-size 1)]
       [min-height (+ picture-size 1)]
       [paint-callback (lambda (canvas dc)
                         (send dc draw-bitmap bitmap 0 0))]))

(define (draw-line start end)
  (send bitmap-dc 
        draw-line
        (xcor-vect start)
        (ycor-vect start)
        (xcor-vect end)
        (ycor-vect end)))

(define window (make-frame (make-vect 0 picture-size)
                           (make-vect picture-size 0)
                           (make-vect 0 (- 0 picture-size))))

(define rhombus (make-frame (make-vect 0 picture-size)
                            (make-vect (* 0.7 picture-size) (* -0.3 picture-size))
                            (make-vect (* 0.3 picture-size) (* -0.7 picture-size))))

(send frame show #t)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))