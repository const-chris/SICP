#lang sicp
(#%require (file "./natural-language-parser.rkt"))

#|
Louis's idea doesn't work. When it tries to parse '(the professor lectures to the student), for example, once it exhausts valid parses, it falls into an infinite loop, as follows:

It attempts to parse the '(lectures to the student with the cat) using the first amb alternative, (parse-word verbs), but after this *unparsed* is not null, so it fails.

It next attempts to parse '(lectures to the student with the cat) using the second amb alternative, (list 'verb-phrase (parse-verb-phrase) (parse-prepositional-phrase)).
The arguments are evaluated left-to-right, so we begin (parse-verb-phrase) again, and this time we are able to use the first amb alternative to parse 'lectures, because the remainder of *unparsed* can be parsed with (parse-prepositional-phrase).

If we ask it to try-again, however, it will backtrack the last branch point--where it succeeded in parsing 'lectures using the first amb alternative. It will follow the next branch, which calls (parse-verb-phrase) again, and it will continue in this manner inifinitely, seeking a new valid parse.
|#

(input-definition
 '(define (parse-verb-phrase)
    (amb (parse-word verbs)
         (list 'verb-phrase
               (parse-verb-phrase)
               (parse-prepositional-phrase)))))

;; uncomment below to test
#| (driver-loop) |#

#|
If we reverse the order of the arguments to amb, the flaw in Louis's idea is exposed immediately. We drop into the inifinite loop before finding valid parses.
|#

(input-definition
 '(define (parse-verb-phrase)
    (amb (list 'verb-phrase
               (parse-verb-phrase)
               (parse-prepositional-phrase))
         (parse-word verbs))))

;; uncomment below to test
#| (driver-loop) |#
