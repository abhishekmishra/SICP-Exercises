#lang scheme

;repeated application of a procedure using composition

;composition of functions
(define (compose f g)
  (lambda (x) (f (g x))))

;repeated
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;test
(define (square x) (* x x))
((repeated square 2) 5)