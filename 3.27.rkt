#lang sicp

;; table implementation
(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (tree? t)
  (and (pair? t) (pair? (entry t)) (= (length t) 3)))

(define (make-record key value) (cons key value))
(define (key record) (car record))
(define (value record) (cdr record))
(define (set-value! record value) (set-cdr! record value))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))


(define (partial-tree elements n)
  (if (= n 0)
      (cons '() elements)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elements left-size)))
          (let ((left-tree (car left-result))
                (non-left-elements (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elements))
                  (right-result (partial-tree (cdr non-left-elements)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elements (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elements))))))))




;; tree methods specific to this table implementation
(define (union-tree set1 set2)
  (define (union-ordered-lists setX setY)
    (cond ((null? setX) setY)
          ((null? setY) setX)
          (else
           (let* ((x (car setX))
                  (xs (cdr setX))
                  (y (car setY))
                  (ys (cdr setY)))
             (cond ((= (key x) (key y)) (cons x (union-ordered-lists xs ys)))
                   ((< (key x) (key y)) (cons x (union-ordered-lists xs setY)))
                   ((> (key x) (key y)) (cons y (union-ordered-lists setX ys))))))))
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (union-ordered-lists list1 list2))))


(define (adjoin-tree x tree)
  (let ((x-tree (make-tree x '() '())))
    (if (not (tree? tree))
        x-tree
        (union-tree x-tree
                    tree))))




(define (assoc k records)
  (cond ((not (tree? records)) false)
        ((= k (key (entry records))) (entry records))
        ((< k (key (entry records))) (assoc k (left-branch records)))
        ((> k (key (entry records))) (assoc k (right-branch records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (value record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-value! record value)
        (set-cdr! table
                  (adjoin-tree (make-record key value)
                               (cdr table)))))
  'ok)

(define (insert-new! key value table)
  (set-cdr! table
            (adjoin-tree (make-record key value)
                         (cdr table)))
  'ok)

(define (make-table)
  (list '*table*))




(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert-new! x result table)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))


(memo-fib 300)
;(fib 300)


#|
           +--------------------------------------------------+
           |                                                  |
global --> | memo-fib: --+    memoize: --+                    |
env        |             |               |                    |
           +-------------|---------------|--------------------+
                         |               |   ^           ^
                         |               |   |           |
(define memo-fib ...)    |               v   |       +---------------------------+
                         |              @=@--+       | f: ---------------+       | <-- E1
                         |              |            +-------------------|-------+
                         |              |                        ^       |   ^
                         |              v                        |       |   |
                         |            parameters: f              |       |   |
                         |            body: (let ((table ... )   |       |   |
                         |                                       |       v   |
                         |                                       |      @=@--+
                         |                                    +--+      |
                         |                                    |         |
                         +------+                             |         v
                                |                             |       parameters: n
                                |                             |       body: (cond ... )
                                |                             |
                                v                   +------------------------+
                               @=@----------------> | table: (list *table*)  | <-- E2
                               |                    +------------------------+
                               |
                               v
                            parameters: x
                            body: (let ((previously-computed-result ... )






           +--------------------------------------------------+
           |                                                  |
global --> | memo-fib: --+    memoize: ...                    |
env        |             |                                    |
           +-------------|------------------------------------+
                         |                   ^        +-------+       +-------+        +-------+        +-------+
                         |                   |        | n: 3  |       | n: 2  |        | n: 1  |        | n: 0  |
                         |                   |        +-------+       +-------+        +-------+        +-------+
(memo-fib 3)             |                   |            |               |                |                |
                         |                   |            |               |                |                |
                         |                   |            v               v                v                v
                         |           +----------------------------------------------------------------------------------+
                         |           | f: ...                                                                           | <-- E1
                         |           +----------------------------------------------------------------------------------+
                         |                         ^
                         |                         |
                         |                         |
                         v                   +---------------------------------------------------------------------------------------+
                        @=@----------------> | table: (list *table*)                                                                 | <-- E2
                        |                    +---------------------------------------------------------------------------------------+
                        |                                         ^                      ^                  ^                  ^
                        |                                         |                      |                  |                  |
                        v                                         |                      |                  |                  |
              parameters: x                                   +-------+              +-------+          +-------+          +-------+
              body: (let ((previously-computed-result ... )   | x: 3  | <-- E3       | x: 2  |          | x: 1  |          | x: 0  |
                                                              +-------+              +-------+          +-------+          +-------+
                                                                ^                      ^                  ^                      ^
                                                                |                      |                  |                      |
                                                                |                      |                  |                      |
                                          +---------------------------------+          |   +---------------------------------+   |
                     @=@----------------> | previously-computed-result: #f  | <-- E4   |   | previously-computed-result: #f  |   |
                     |                    +---------------------------------+          |   +---------------------------------+   |
                     |                                                                 |                                         |
                     v                                           +---------------------------------+    +---------------------------------+
                 parameters: result                              | previously-computed-result: #f  |    | previously-computed-result: #f  |
                 body: (insert-new! ...)                         +---------------------------------+    +---------------------------------+





           +--------------------------------------------------+
           |                                                  |
global --> | memo-fib: --+    memoize: ...                    |
env        |             |                                    |
           +-------------|------------------------------------+
                         |                   ^
                         |                   |
                         |                   |
(memo-fib 3)             |                   |
                         |                   |
                         |                   |
                         |           +--------------------------+
                         |           | f: ...                   | <-- E1
                         |           +--------------------------+
                         |                         ^
                         |                         |
                         |                         |
                         v                   +-------------------------------------------------------------------------+
                        @=@----------------> | table: (*table* (2 . 1) (1 . 1) (0 . 0))                                | <-- E2
                        |                    +-------------------------------------------------------------------------+
                        |                                         ^                      ^                  ^
                        |                                         |                      |                  |
                        v                                         |                      |                  |
              parameters: x                                   +-------+              +-------+          +-------+
              body: (let ((previously-computed-result ... )   | x: 3  | <-- E3       | x: 2  |          | x: 1  |
                                                              +-------+              +-------+          +-------+
                                                                ^                      ^                  ^
                                                                |                      |                  |
                                                                |                      |                  |
                                          +---------------------------------+          |   +---------------------------------+
                     @=@----------------> | previously-computed-result: #f  | <-- E4   |   | previously-computed-result: 1   |
                     |                    +---------------------------------+          |   +---------------------------------+
                     |                                                                 |
                     v                                           +---------------------------------+
                 parameters: result                              | previously-computed-result: 1   |
                 body: (insert-new! ...)                         +---------------------------------+
|#

#|
It would not work to define memo-fib as (memoize fib),
because the recursive calls inside fib reference fib itself,
which does not contain a call to memoize.
|#
