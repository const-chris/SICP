#lang sicp

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
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else
             (error "unknown dispatch -- MAKE-QUEUE" m))))
    dispatch))

(define (front-ptr q) (q 'front-ptr))
(define (rear-ptr q) (q 'rear-ptr))
(define (empty-queue? q) (q 'empty-queue?))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) ((q 'delete-queue!)))

;; tests
(newline)


(define q1 (make-queue))
(display "q1 = ")
q1

(display "(insert-queue! q1 'a) = ")
(insert-queue! q1 'a)

(display "(empty-queue? q1) = ")
(empty-queue? q1)

(display "(insert-queue! q1 'b) = ")
(insert-queue! q1 'b)

(display "(delete-queue! q1) = ")
(delete-queue! q1)

(display "(delete-queue! q1) = ")
(delete-queue! q1)

(display "(empty-queue? q1) = ")
(empty-queue? q1)





(newline)
