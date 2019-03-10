
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; sequence takes 3 arguments, low, high, and stride and produces a list from low to high,
;; including low and possibly high, separated by stride and in sorted order

(define (sequence low high stride)
  (if ( > low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; string-append-map takes a list of strings xs and a string suffix and
;; returns a list of strings. Each element of theoutput should be the corresponding element
;; of the input with the input appended to the suffix. You must use the Racket functions map
;; string append

(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix))
         xs))



