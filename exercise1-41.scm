#lang scheme

;the double procedure, which given a procedure argument,
;returns a procedure to apply the given procedure twice on its input
(define (double g)
  (lambda (x) (g (g x))))

;test double increment
(define (inc x) (+ x 1))
((double inc) 1)

;every time double is applied, the amount of increment is squared
(((double (double double)) inc) 5)