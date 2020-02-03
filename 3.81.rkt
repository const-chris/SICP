#lang sicp
(#%require (file "utils/repeat.rkt"))
(#%require (file "utils/rand-update.rkt"))
(#%require (file "stream-utils.rkt"))




(define (rand at)
  (lambda (request-stream)
    (if (stream-null? request-stream)
        '()
        (let* ((request (stream-car request-stream))
               (next
                 (cond ((eq? request 'generate) (rand-update at))
                       ((and (pair? request)
                             (eq? (car request) 'reset)
                             (real? (cdr request)))
                        (cdr request))
                       (else (error "invalid request -- RAND" request)))))
          (cons-stream
            next
            ((rand next) (stream-cdr request-stream)))))))




;; tests
;#|
(define (rand-reset n) (list (cons 'reset n)))

(define requests (apply make-stream (append (repeat 5 'generate)
                                            (rand-reset 1)
                                            (repeat 4 'generate)
                                            (rand-reset 5)
                                            (repeat 5 'generate)
                                            (rand-reset 5)
                                            (repeat 4 'generate)
                                            (rand-reset 71))))


(define bad-requests (make-stream 'generate 'something 'generate))


(stream-take 22 ((rand 1) requests))
;(stream-take 3 ((rand 1) bad-requests))
;|#
