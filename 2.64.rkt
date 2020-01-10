#lang racket
(newline)
(provide list->tree)

(define (make-tree entry left right)
  (list entry left right))




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

#|
Partial tree dvides the list into 4 parts:
   1) elements preceding the (n/2)th
   2) the (n/2)th
   3) elements between the (n/2)th and the nth
   4) and elements after the nth.

A tree is constructed from the first three of those parts (the first and third part having been recursively
made into trees themselves), and that tree is consed onto the fourth part.
In the terminating case, '() is consed onto the list of elements yet to be processed.
This '() becomes either the left or right branch (part1 or part3 above) of a leaf node.

Because the unused part of the list is carriend along in the remaining-elements variable, no computation is repeated.
This makes the procedure linear-recursive, and since all operations other than recursive calls are O(1),
it has an order of growth in the number of steps required to convert an ordered list into a binary tree of O(n).

(list->tree '(1 3 5 7 9 11))

             5
          /    \
         1      9
          \   /   \
          3  7     11
|#



(newline)