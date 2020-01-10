#lang racket
(require "huffman-tree.rkt")
(provide generate-huffman-tree)


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs)
                                                    (cadr pairs))
                                    (cddr pairs)))))




#|
(newline)




(define sample-pairs '((A 4) (B 2) (C 1) (D 1)))

(define test-tree (generate-huffman-tree sample-pairs))

test-tree
sample-tree

(decode sample-message test-tree)




(newline)
;|#