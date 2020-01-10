#lang racket
(require "2.46.rkt" "2.48.rkt" "2.50.rkt" "2.51.rkt" "picture-language.rkt" "chapter2.rkt")
(provide spiral wave)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
                (draw-line ((frame-coord-map frame) (start-segment segment))
                           ((frame-coord-map frame) (end-segment segment))))
              segment-list)))


(define (make-segments-list . vs)
  (if (null? (cdr vs))
      nil
      (cons (make-segment (car vs) (cadr vs))
            (apply make-segments-list (cdr vs)))))

(define spiral
  (segments->painter (make-segments-list (make-vect 0.0 0.0)
                                         (make-vect 1.0 0.0)
                                         (make-vect 1.0 1.0)
                                         (make-vect 0.0 1.0)
                                         (make-vect 0.0 0.1)
                                         (make-vect 0.9 0.1)
                                         (make-vect 0.9 0.9)
                                         (make-vect 0.1 0.9)
                                         (make-vect 0.1 0.2)
                                         (make-vect 0.8 0.2)
                                         (make-vect 0.8 0.8)
                                         (make-vect 0.2 0.8)
                                         (make-vect 0.2 0.3)
                                         (make-vect 0.7 0.3)
                                         (make-vect 0.7 0.7)
                                         (make-vect 0.3 0.7)
                                         (make-vect 0.3 0.4)
                                         (make-vect 0.6 0.4)
                                         (make-vect 0.6 0.6)
                                         (make-vect 0.4 0.6)
                                         (make-vect 0.4 0.5)
                                         (make-vect 0.5 0.5))))

(define a
  (segments->painter (make-segments-list (make-vect 0 0)
                                         (make-vect 0 1)
                                         (make-vect 1 1)
                                         (make-vect 1 0)
                                         (make-vect 0 0))))

(define b
  (segments->painter (list (make-segment (make-vect 0 0)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 0)
                                         (make-vect 0 1)))))

(define c
  (segments->painter (make-segments-list (make-vect 0 0.5)
                                         (make-vect 0.5 1)
                                         (make-vect 1 0.5)
                                         (make-vect 0.5 0)
                                         (make-vect 0 0.5))))

(define wave
  (segments->painter (list (make-segment (make-vect 0.4 0)
                                         (make-vect 0.5 0.3))
                           (make-segment (make-vect 0.6 0)
                                         (make-vect 0.5 0.3))  ; inner legs
                           (make-segment (make-vect 0.25 0)
                                         (make-vect 0.35 0.55))
                           (make-segment (make-vect 0.75 0)
                                         (make-vect 0.6 0.5))  ; outer legs
                           (make-segment (make-vect 1 0.15)
                                         (make-vect 0.6 0.5))
                           (make-segment (make-vect 1 0.3)
                                         (make-vect 0.7 0.7))  ; right arm
                           (make-segment (make-vect 0.35 0.55)
                                         (make-vect 0.3 0.65)) ; torso
                           (make-segment (make-vect 0.3 0.65)
                                         (make-vect 0.15 0.45))
                           (make-segment (make-vect 0.15 0.45)
                                         (make-vect 0 0.7))    ; lower left arm
                           (make-segment (make-vect 0 0.85)
                                         (make-vect 0.15 0.65))
                           (make-segment (make-vect 0.15 0.65)
                                         (make-vect 0.3 0.7))  ; upper left arm
                           (make-segment (make-vect 0.3 0.7)
                                         (make-vect 0.425 0.7))
                           (make-segment (make-vect 0.7 0.7)
                                         (make-vect 0.575 0.7))  ; shoulders
                           (make-segment (make-vect 0.575 0.7)
                                         (make-vect 0.625 0.9))
                           (make-segment (make-vect 0.625 0.9)
                                         (make-vect 0.6 1))    ; right side of head
                           (make-segment (make-vect 0.425 0.7)
                                         (make-vect 0.375 0.9))
                           (make-segment (make-vect 0.375 0.9)
                                         (make-vect 0.4 1))    ; left side of head
                           (make-segment (make-vect 0.575 0.85)
                                         (make-vect 0.55 0.825))
                           (make-segment (make-vect 0.55 0.825)
                                         (make-vect 0.45 0.825))
                           (make-segment (make-vect 0.425 0.85)
                                         (make-vect 0.45 0.825)) ; smile
                           )))



;((beside (below wave wave)
;        (flip-vert (below wave wave)))
;window)