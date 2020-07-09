#lang sicp
(#%require (file "./4.35.rkt"))
(#%provide (all-from (file "./4.35.rkt"))
           (all-defined))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
           (list 'noun-phrase
                 noun-phrase
                 (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
           (list 'verb-phrase
                 verb-phrase
                 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))


(input-definition '(define nouns '(noun student professor cat class)))
(input-definition '(define verbs '(verb studies lectures eats sleeps)))
(input-definition '(define articles '(article the a)))
(input-definition '(define prepositions '(prep for to in by with)))

(input-definition '(define (parse-word word-list)
                     (require (not (null? *unparsed*)))
                     (require (memq (car *unparsed*) (cdr word-list)))
                     (let ((found-word (car *unparsed*)))
                      (set! *unparsed* (cdr *unparsed*))
                      (list (car word-list) found-word))))

(input-definition '(define *unparsed* '()))

(input-definition '(define (parse input)
                     (set! *unparsed* input)
                     (let ((sent (parse-sentence)))
                       (require (null? *unparsed*)) sent)))

(input-definition '(define (parse-sentence)
                     (list 'sentence (parse-noun-phrase) (parse-verb-phrase))))

(input-definition '(define (parse-noun-phrase)
                     (define (maybe-extend noun-phrase)
                       (amb noun-phrase
                            (maybe-extend
                              (list 'noun-phrase
                                    noun-phrase
                                    (parse-prepositional-phrase)))))
                     (maybe-extend (parse-simple-noun-phrase))))

(input-definition '(define (parse-simple-noun-phrase)
                     (list 'simple-noun-phrase
                           (parse-word articles)
                           (parse-word nouns))))

(input-definition '(define (parse-verb-phrase)
                     (define (maybe-extend verb-phrase)
                       (amb verb-phrase
                            (maybe-extend
                              (list 'verb-phrase
                                    verb-phrase
                                    (parse-prepositional-phrase)))))
                     (maybe-extend (parse-word verbs))))

(input-definition '(define (parse-prepositional-phrase)
                     (list 'prep-phrase
                           (parse-word prepositions)
                           (parse-noun-phrase))))

