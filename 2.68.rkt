#lang racket
(require "huffman-tree.rkt")
(provide encode)

(define (is-element-of? x items)
  (cond ((null? items) false)
        ((equal? x (car items)) true)
        (else (is-element-of? x (cdr items)))))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (encode-symbol char tree)
  (define (on-branch? symbol branch)
    (is-element-of? symbol
                    (symbols (branch tree))))
  (cond ((and (leaf? tree)
              (eq? char (symbol-leaf tree)))
         '())
        ((on-branch? char left-branch) (cons 0 (encode-symbol char (left-branch tree))))
        ((on-branch? char right-branch) (cons 1 (encode-symbol char (right-branch tree))))
        (else (error "character not present in encoding tree -- ENCODE-SYMBOL" char))))




(encode '(A D A B B C A) sample-tree)