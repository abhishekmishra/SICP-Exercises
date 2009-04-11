#lang mzscheme

;; design a multiplication procedure like fast-expt
;; using double, halve, and addition only - that ues o(log(n)) time

;; recursive

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

(define (* a b)
  (write (list '* a b))
  (newline)
  (cond ((= a 0) 0)
        ((= a 1) b)
        ((even? a) (* (halve a) (double b)))
        (else (+ (* (- a 1) b) b))))

(write (* 110000 1000))
(newline)
(write (* 1 25))
(newline)
(write (* 0 23))
(newline)
(write (* 23 0))