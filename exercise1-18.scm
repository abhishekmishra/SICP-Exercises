#lang mzscheme

;; design a multiplication procedure like fast-expt
;; using double, halve, and addition only - that ues o(log(n)) time

;; iterative

(define (double a)
  (+ a a))

(write (double 28))
(newline)

(define (halve a)
  (if (even? a)
      (/ a 2)
      'noteven))

(write (halve 28))
(newline)
(write (halve 27))
(newline)

