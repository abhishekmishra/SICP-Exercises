#lang scheme

;alternate implementation of pairs
;pairs of integers a b represented as 2^a*3^b
;cons, car and cdr

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (if (= (remainder z 2) 0)
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= (remainder z 3) 0)
      (+ 1 (cdr (/ z 3)))
      0))

(define x (cons 1 2))
(car x)
(cdr x)

(define x1 (cons 101 211))
(car x1)
(cdr x1)