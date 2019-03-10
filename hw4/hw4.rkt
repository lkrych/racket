
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

;; list-nth-mod takes a list xs and a number n. If the number is negative
;; terminate the computation with an error, else if the list is
;; empty, terminate the list wtih an error, else return the ith
;; element of the list where we count from zero and i is the remainder
;; when dividing n by the list's length
(define (list-nth-mod xs n)
  (cond
    [(< n 0)  (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [list-ref xs (modulo n (length xs))]))

;; define some streams for testing
(define ones (lambda () (cons 1 ones)))
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

;; stream-for-n-steps takes a stream s and a number n
;; it returns a list holding the first n values produced by s in order, assume n is non-negative

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))


;; funny-number-stream is like the stream of natural numbers except numbers
;; divisible by 5 are negated

(define (funny-number-stream)
  (define (f x) (cons (if (= 0 (remainder x 5)) (- x) x)
                      (lambda () (f (+ x 1)))))
  (f 1))

;; dan-then-dog is a stream that alterntates between "dan.jpg" and "dog.jpg", starting
;; with dan.

(define (dan-then-dog)
  (define (f this-photo)
    (define next-photo (if (string=? "dan.jpg" this-photo) "dog.jpg" "dan.jpg"))
    (cons this-photo
    (lambda () (f next-photo))))
(f "dan.jpg"))

;; stream-add-zero takes a stream s and returns another stream.
;; if s would produce v for its ith element, then stream-add-zero s would produce
;; the pair (0 . v) for its ith element

(define (stream-add-zero s)
  (define (f x) (cons
                 (cons 0 (car (x)))
                 (lambda () (f (cdr (x))))))
    (lambda () (f s)))