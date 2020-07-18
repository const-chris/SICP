#lang sicp
(#%require (file "./stream-utils.rkt"))
(#%require (file "./microshaft-db.rkt"))
(#%require (file "./4.65.rkt"))

#|
Ben has just realized that his simple accumulation scheme won't work in the presence of rules.
With rules, duplicate responses can be returned, as we saw in 4.65, because there may be multiple ways of satisfying a rule body that converge upon instantiating the original query.

Imagine, for example, Ben wanted to query for the sum of the salaries of all the wheels in the company.
His query would look like:

(sum ?amount (and (wheel ?x)
                  (salary ?x ?amount)))

As we've already seen, (wheel (Warbucks Oliver)) will be in the stream of frames passed to (salary ?x ?amount) four times, and so there will be four instances of his salary in the final stream of frames from which Ben's algorithm extracts the instances of ?amount.

One way Ben could salvage his idea is to provide a unique key to each accumulation query.
In the example above, perhaps that would be the employee's name, and his query would look like:

(sum-by-key ?name ?amount (and (wheel ?name)
                               (salary ?name ?amount)))

Then before accumulation, there would be a filtering step whereby only the first occurence of each key in the stream of frames coming from qeval would be passed on to the accumulator. One possible implementation follows.
|#

(define (get-value-for-key key frame)
  (let ((value (cdr (assoc key frame))))
    (if (var? value)
        (get-value-for-key (contract-question-mark value) frame)
        value)))

(define (nub-by-key key frames)
  (let ((seen '()))
    (stream-filter
      (lambda (frame)
        (let* ((value (get-value-for-key key frame))
               (unseen (not (member value seen))))
          (if unseen (set! seen (cons value seen)))
          unseen))
      frames)))

(define (contract-keys frame)
  (map (lambda (kvs)
         (cons (contract-question-mark (car kvs))
               (cdr kvs)))
       frame))

(define (extract-param param frame)
  (cdr (assoc param frame)))

(define (query-accumulate op zero)
  (lambda (key param query)
    (let ((q (query-syntax-process query)))
      (stream-accumulate op
                         zero
                         (stream-map (lambda (frame) (extract-param param frame))
                                     (nub-by-key key
                                                 (stream-map contract-keys
                                                             (qeval q (singleton-stream '())))))))))

(define sum-by-key (query-accumulate + 0))

;#| tests
(newline)

(sum-by-key '?name '?amount '(and (wheel ?name)
                                  (salary ?name ?amount)))


(sum-by-key '?x '?amount '(and (job ?x (computer programmer))
                               (salary ?x ?amount)))
;|#
