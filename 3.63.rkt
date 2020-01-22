#lang sicp
(#%require racket/trace)
(#%require (file "stream-utils.rkt"))

(define (average x y)
  (/ (+ x y) 2))

(trace-define (sqrt-improve guess x)
              (average guess (/ x guess)))




;; original sqrt-stream
(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

;; Louis' sqrt-stream
(define (louis-sqrt-stream x)
  (cons-stream 1.0 (stream-map
                     (lambda (guess)
                       (sqrt-improve guess x))
                     (louis-sqrt-stream x))))




#|
The original version creates an environment variable binding for the stream, which memo-proc can reference. Louis's version creates a new inner procedure call each time delay is called, which is not the *same* procedure as its enclosing call, so memoization doesn't help.
If memo-proc were not used, no additional redundancy would be introduced by Louis' version.
|#




;; tests
;#|
(trace sqrt-stream)
(stream-take 5 (sqrt-stream 2))

(trace louis-sqrt-stream)
(stream-take 5 (louis-sqrt-stream 2))
;|#
