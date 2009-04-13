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

;smoothing function
;------------------
(define dx 0.00001)

;smooth
(define (smooth f)
  (lambda (x) 
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

;n-fold smoothed function
(define (n-fold-smoothed f n) 
  (repeated (smooth f) n))

;test
(define (square x) (* x x))
((n-fold-smoothed square 2) 5)