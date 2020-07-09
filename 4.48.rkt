#lang sicp
(#%require (file "./natural-language-parser.rkt"))

(input-definition
  '(define adjectives '(adjective happy tired bored)))

(input-definition
  '(define adverbs '(adverb quickly sleepily)))

(input-definition
  '(define (parse-simple-noun-phrase)
     (cons 'simple-noun-phrase
           (cons (parse-word articles)
                 (parse-word-with-maybe-descriptors nouns adjectives)))))

(input-definition
 '(define (parse-word-with-maybe-descriptors word-list descriptor-list)
    (amb (list (parse-word word-list))
         (cons (parse-word descriptor-list)
               (parse-word-with-maybe-descriptors word-list descriptor-list)))))

(input-definition
 '(define (parse-verb-phrase)
    (define (maybe-extend verb-phrase)
      (amb verb-phrase
           (maybe-extend
             (cons 'verb-phrase
                   (append verb-phrase
                           (list (parse-word adverbs)))))
           (maybe-extend
             (list 'verb-phrase
                   verb-phrase
                   (parse-prepositional-phrase)))))
    (maybe-extend (parse-word-with-maybe-descriptors verbs adverbs))))

(driver-loop)
