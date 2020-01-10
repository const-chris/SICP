#lang racket
(provide make-stack
         empty-stack?
         peek
         push!
         pop!)

(define (make-stack)
  (let ((top-ptr '()))
    (define (stack) top-ptr)
    (define (empty-stack?) (null? top-ptr))
    (define (peek)
      (if (empty-stack?)
          (error "PEEK called with an empty stack" (stack))
          (car top-ptr)))
    (define (push! item)
      (set! top-ptr (cons item top-ptr))
      (stack))     
    (define (pop!)
      (cond ((empty-stack?)
             (error "POP! called with an empty stack" (stack)))
            (else
             (set! top-ptr (cdr top-ptr))
             (stack))))
    (define (dispatch m)
      (cond ((eq? m 'empty-stack?) (empty-stack?))
            ((eq? m 'peek) (peek))
            ((eq? m 'push!) push!)
            ((eq? m 'pop!) pop!)
            (else
             (error "unknown dispatch -- MAKE-STACK" m))))
    dispatch))

(define (empty-stack? s) (s 'empty-stack?))
(define (peek s) (s 'peek))
(define (push! s item) ((s 'push!) item))
(define (pop! s) ((s 'pop!)))


#|
;; tests

(define s (make-stack))

(push! s 1)
(push! s 2)
(peek s)
(push! s 3)
(pop! s)
(pop! s)
(peek s)

;|#