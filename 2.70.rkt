#lang racket
(require "huffman-tree.rkt"
         "2.68.rkt"
         "2.69.rkt")


(define pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define tree (generate-huffman-tree pairs))

(define lyrics '(GET A JOB SHA NA NA NA NA NA NA NA NA
                       GET A JOB SHA NA NA NA NA NA NA NA NA
                       WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                       SHA BOOM))

(define bits (encode lyrics tree))

(define bits-per-symbol (log (length pairs) 2))


(display "\nEncoded lyrics:\n")
(display bits)

(display "\n\n\nEncoding the lyrics requires ")
(display (length bits))
(display " bits.\n\nWith a fixed-length code it would have required a minimum of log_2 n bits per symbol,
where n = the number of unique symbols.\n\nHere that is log_2 ")
(display (length pairs))
(display " = ")
(display bits-per-symbol)
(display " bits per symbol.\n\nThe total number of symbols in the lyrics is ")
(display (length lyrics))
(display ", so the minimum number of bits that would be required
to represent the lyrics using a fixed-length code is ")
(display (* (length lyrics) bits-per-symbol))
(display ".\n\n")

  