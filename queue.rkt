#lang sicp
(#%provide make-queue
           front-queue
           empty-queue?
           insert-queue!
           delete-queue!)


(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (queue) (cons front-ptr rear-ptr))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" (queue))
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (queue))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               (queue)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" (queue)))
            (else
             (set! front-ptr (cdr front-ptr))
             (queue))))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else
             (error "unknown dispatch -- MAKE-QUEUE" m))))
    dispatch))

(define (front-queue q) (q 'front-queue))
(define (empty-queue? q) (q 'empty-queue?))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) ((q 'delete-queue!)))



#|
;; tests
(define q (make-queue))
(insert-queue! q 3)
(insert-queue! q 2)
(front-queue q)
(delete-queue! q)
(front-queue q)
(empty-queue? q)
(delete-queue! q)
(empty-queue? q)



(newline)
;|#



