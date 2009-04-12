#lang scheme

;section on procedures as arguments

;PART-A

;the sum procedure
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;factorial interms of product

;increment function
(define (inc x)
  (+ 1 x))

;identity
(define (identity x) x)

;factorial function
(define (factorial n)
  (product identity 1 inc n))

;test
(factorial 3)
(factorial 6)
(factorial 9)

;compute pi using
;pi/4 = 2*4*4*6*6*8..../3*3*5*5*7*7
(define (pi-approx n)
  (define (piterm x)
    (/ (* x (+ x 2.0)) (* (+ x 1.0) (+ x 1.0))))
  (define (pinext x)
    (+ x 2.0))
  (* 4.0 (product piterm 2.0 pinext (* 2.0 n))))

;test
(pi-approx 1)
(pi-approx 100)             
(pi-approx 10000)             

;PART-B

;iterative form of product
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1.0))
      
;compute pi using
;pi/4 = 2*4*4*6*6*8..../3*3*5*5*7*7
(define (pi-approx-iter n)
  (define (piterm x)
    (/ (* x (+ x 2.0)) (* (+ x 1.0) (+ x 1.0))))
  (define (pinext x)
    (+ x 2.0))
  (* 4.0 (product-iter piterm 2.0 pinext (* 2.0 n))))

;test
(pi-approx-iter 1)
(pi-approx-iter 100)             
(pi-approx-iter 10000)     