#lang sicp
(#%provide (all-defined))

(define eval-in-underlying-scheme eval)
(define user-initial-environment (scheme-report-environment 5))
