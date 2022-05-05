#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; cons adds an element into a list
(cons "peanut" "butter and jelly")
; give the first element of a list
(car (cons 1 2))
; give list with every element but the first element
(cdr (cons 1 2))

(define lat?
  (lambda(l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; list of atoms
(lat? '("bacon and eggs"))