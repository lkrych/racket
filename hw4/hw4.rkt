
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

;; stream-from-list takes a list and creates a stream that cycles through that stream
(define (stream-from-list l)
  (define (f idx) (cons
                   (list-ref l idx)
                   (lambda () (f (+ idx 1)))))
  (lambda () (f l)))

;; cycle-lists takes two lists xs and ys and returns a stream.
;; The elements produced by the stream are pairs where the first part is from xs
;; and the second part are from ys. The stream cycles forever through the lists

(define (cycle-lists xs ys)
  (define streamx (stream-from-list xs))
  (define streamy (stream-from-list ys))
  (define (f sx sy) (cons
                   (cons (car (sx)) (car (sy)))
                   (lambda () (f (cdr (sx)) (cdr (sy))))))
  (lambda () (f streamx streamy)))

;; vector-assoc takes a value v and a vector vec
;; it should behave like Racket's assoc library function
;; except it processes a vector (array) instead of list
;; it allows vector elements not to be pairs, in which case it skips them
;; it always takes exactly two arguments. Process the vector elements in order starting from 0
;; Return #f if no vector element is a pair with a car field equal to v, else return the first pair
;; with an equal car field

;; library assoc v list locates the first element of lst whose car is equal to v

(define (vector-assoc v vec)
  (define (f current-idx)
    (cond
      [(= (vector-length vec) (- current-idx 1)) #f] ;; if we've gotten to the end of the array and found no matches
     [else
      (let ([current-pair (vector-ref vec current-idx)])
       (cond
         [(pair? current-pair) ;; if there is a pair, check if they are equal, otherwise skip
          (cond
            [(equal? (car current-pair) v) current-pair]
            [else (f (+ current-idx 1))])]
         [else (f (+ current-idx 1))]))])) ;;skip and keep searching
  (f 0))

;; cached-assoc takes a list xs and a number n and returns a function that takes
;; one argument v and returns the same thing that (assoc v xs) would return. However,
;; you should use an n-element cache of recent results to possibly make this function
;; faster than just calling xs
;; the cache must be a Racket vector of length n that is created by the call to cached-assoc
;; and used-and-possibly-mutated each time the function returned by cached-assoc is called

;; The cache starts empty. When the function returned by cached-assoc is called, it first checks
;; the cache for an answer. If it is not there, it uses assoc and xs to get the answer
;; and if the result is not #f, it adds the pair to the cache before returning.
;; cache slots are used in a round-robin fashion, the first time a pair is added to the cache
;; it is put in position 0, the next pair is put in position 1, when it wraps back around, the
;; 0th pair is replaced

(define (cached-assoc xs n)
  ;; make cache
  (letrec ([ cache (make-vector n) ]
        [cache-idx 0]
        [f (lambda(v) ;;define assoc function
      (let ([cache-hit (vector-assoc v cache) ])
      (cond
        [(cache-hit) cache-hit] ;;check cache
        [else
         (let ([assoc-result (assoc v xs)])
           (cond
             [(assoc-result)
              (begin (vector-set! cache cache-idx assoc-result)
                   (set! cache-idx (modulo (+ cache-idx 1) n))
                   assoc-result)]
             [else #f]))])))]
    )
    f)) ;; return assoc function
  