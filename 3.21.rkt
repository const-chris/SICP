#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
   (cond ((empty-queue? queue)
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
         (else
          (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

#|
The cdr of the queue points to the tail of the list, while the car points to
the head of the list. The head pointer therefore points to the complete list, which
is what the Lisp printer shows. Because both pointers contain a reference to the last
item (the head via cdring down the list, and the tail directly) it is printed twice.
And because our delete-queue! procedure doesn't check to see if the queue is empty after
moving the front pointer, the rear pointer maintains a reference to the last item that
was in the queue after it is emptied.
|#

(define (print-queue q)
  (display (car q)))
  (newline)




;; tests
(define q1 (make-queue))
(display "q1 = ")
q1
(newline)

(display "(print-queue (insert-queue! q1 'a)) = ")
(print-queue (insert-queue! q1 'a))
(newline)

(display "(print-queue (insert-queue! q1 'b)) = ")
(print-queue (insert-queue! q1 'b))
(newline)

(display "(print-queue (delete-queue! q1)) = ")
(print-queue (delete-queue! q1))
(newline)

(display "(print-queue (delete-queue! q1)) = ")
(print-queue (delete-queue! q1))
(newline)

(newline)
