#lang sicp
(#%require (file "./natural-language-parser.rkt"))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (let ((shuffled (shuffle cprocs)))
        (define (try-next choices)
          (if (null? choices)
              (fail)
              ((car choices)
               env
               succeed
               (lambda () (try-next (cdr choices))))))
        (try-next shuffled)))))


(define (shuffle xs)
  (if (null? xs)
      '()
      (let ((choice (random-element xs)))
        (cons choice (shuffle (remove choice xs))))))

(define (random-element xs)
  (list-ref xs (random (length xs))))

(define (remove x xs)
  (cond ((null? xs) '())
        ((equal? x (car xs)) (cdr xs))
        (else (cons (car xs) (remove x (cdr xs))))))


(define (install-amb-analyze-ramb-package)
  (display "installing amb-analyze-ramb-package... ")
  (put 'amb-analyze 'ramb analyze-ramb))

(install-amb-analyze-ramb-package)


(input-definition '(define (parse-word word-list)
                     (define (random-in-word-list words)
                       (if (null? words)
                           (amb)
                           (ramb (car words) (random-in-word-list (cdr words))))) ;; changed
                     (list (car word-list) (random-in-word-list (cdr word-list)))))

(input-definition '(define (generate-sentences) (parse-sentence)))

(input-definition '(define (parse-noun-phrase)
                     (define (maybe-extend noun-phrase)
                       (ramb noun-phrase  ;; changed
                             (maybe-extend
                               (list 'noun-phrase
                                     noun-phrase
                                     (parse-prepositional-phrase)))))
                     (maybe-extend (parse-simple-noun-phrase))))

(input-definition '(define (parse-verb-phrase)
                     (define (maybe-extend verb-phrase)
                       (ramb verb-phrase  ;; changed
                             (maybe-extend
                               (list 'verb-phrase
                                     verb-phrase
                                     (parse-prepositional-phrase)))))
                     (maybe-extend (parse-word verbs))))

(driver-loop)
