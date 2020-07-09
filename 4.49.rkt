#lang sicp
(#%require (file "./natural-language-parser.rkt"))

(input-definition '(define (parse-word word-list)
                     (define (random-in-word-list words)
                       (amb (car words) (random-in-word-list (cdr words))))
                     (list (car word-list) (random-in-word-list (cdr word-list)))))

(input-definition '(define (generate-sentences) (parse-sentence)))

(driver-loop)

#|
With these definitions, the sentences generated are of the form:
The student studies.
The student studies for the student.
The student studies for the student for the student.
The student studies for the student for the student for the student.
etc.
|#
