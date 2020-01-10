#lang sicp

#|
Each subtable is a list of the form [key, tree]
To lookup a value in the table is essentially the same as in the unordered list implementation,
except that assoc can traverse the tree in log n steps instead of n steps (by checking whether
the target value is <, >, or = the value at the current node, then choosing the left branch,
right branch or current value to narrow down it's search).

To insert a value at a list of keys, we traverse the current tree using the assoc procedure described
above.
If the current key does not exist, we add a subtable for that key containing an empty tree if there are
more keys in the list, a tree containing the value and two empty branches if it is the last key in the
list.
If the current key already exists and we are at the last key in the list, we overwrite the tree for that
key with the value.
If the current key already exists and we are not at the last key in the list, we iterate using (cdr keys)
as the new list of keys and the subtable whose key is the one found by assoc as the new subtable.
|#

;; generic comparison procedures and utils
(define (string-join sep items)
  (cond ((null? items) "")
        ((null? (cdr items)) (car items))
        (else
         (string-append (car items) sep (string-join sep (cdr items))))))

(define (->string x)
  (cond ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((string? x) x)
        ((pair? x) (string-append "'(" (string-join " " (map ->string x)) ")"))
        (else
         (error ("unknown type -- ->STRING" x)))))

(define (lt? x y)
  (string<? (->string x) (->string y)))

(define (gt? x y)
  (string>? (->string x) (->string y)))




;; tree methods
(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (tree? t)
  (and (pair? t) (= (length t) 3)))

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
                  (x-key (key-record x))
                  (xs (cdr setX))
                  (y (car setY))
                  (y-key (key-record y))
                  (ys (cdr setY)))
             (cond ((equal? x-key y-key) (cons x (union-ordered-lists xs ys)))
                   ((lt? x-key y-key) (cons x (union-ordered-lists xs setY)))
                   ((gt? x-key y-key) (cons y (union-ordered-lists setX ys))))))))
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (list->tree (union-ordered-lists list1 list2))))


(define (adjoin-tree x tree)
  (let ((x-tree (make-tree x '() '())))
    (if (not (tree? tree))
        x-tree
        (union-tree x-tree
                    tree))))




;; table methods
(define (make-record key value) (cons key value))
(define (key-record r) (car r))
(define (value-record r) (cdr r))

(define (make-subtable keys value)
  (if (null? (cdr keys))
      (make-record (car keys) value)
      (make-record (car keys)
                   (make-tree (make-subtable (cdr keys) value)
                              '()
                              '()))))

(define (assoc key records)
  (cond ((not (tree? records)) false)
        ((equal? key (key-record (entry records))) (entry records))
        ((lt? key (key-record (entry records)))
         (assoc key (left-branch records)))
        ((gt? key (key-record (entry records)))
         (assoc key (right-branch records)))))




(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter keys subtable)
        (let ((record (assoc (car keys) (value-record subtable))))
          (cond ((not record) false)
                ((null? (cdr keys)) (value-record record))
                (else
                 (iter (cdr keys) record)))))
      (iter keys local-table))
  
    (define (insert! keys value)
      (define (iter keys subtable)
        (let ((record (assoc (car keys) (value-record subtable))))
          (cond ((not record)    
                 (set-cdr! subtable
                           (adjoin-tree (make-subtable keys value)
                                        (cdr subtable))))
                ((null? (cdr keys))
                 (set-cdr! record value))
                (else
                 (iter (cdr keys) record)))))              
      (iter keys local-table)
      (string-append "inserted " (->string value) " at " (->string keys)))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


     

;; tests
(newline)


(define t (make-table))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))


(put '(a) 1)
(get '(a))
(put '(11) 11)
(get '(11))
(put '(a b) 1)
(put '(a c) 2)
(get '(a b))
(get '(a c))
(put '(1 2) 3)
(get '(1 2))
(put '(1 3 4 5) 4)
(get '(1 3 4 5))
(get '(1 3))
(get '(1 3 4))
(get '(1))
(get '(1 3 4 5))
(put '(1 3) 2)
(get '(1 3))
(get '(1 3 4 5))

(newline)
(put '(name first) "Clara")
(put '(name last) "Bechtel")
(put '(awesomeness) "Extraordinary")
(put '(address street number) 1063)
(put '(address street name) "N Clarkson")
(put '(address street suffix) "St.")
(put '(address city) "Denver")
(put '(address apt) 11)
(put '(address state) "CO")
(put '(address zip) 80218)

(newline)
(get '(name))
(get '(address))
(get '(awesomeness))




(newline)