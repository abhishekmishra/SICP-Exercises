#lang scheme

;accumulate procedure as a general notion of the product and sum 
;procedures in the previous 3 exercises.

;a recursive accumulate
(define (accumulate combine null-value term a next b)
  (if (> a b)
      null-value
      (combine 
       (term a)
       (accumulate combine null-value term (next a) next b))))

;the sum procedure
(define (sum term a next b)
  (accumulate + 0 term a next b))

;increment procedure
(define (inc n) (+ n 1))

;cube
(define (cube a) (* a a a))

;sum of cubes
(define (sum-cubes a b)
  (sum cube a inc b))

;test
(sum-cubes 1 10)

;identity 
(define (identity a) a)

;sum integers
(define (sum-integers a b)
  (sum identity a inc b))

;test
(sum-integers 1 10)

;a series by Leibniz that converhes to pi/8
; 1/(1*3) + 1/(5*7) + 1/(9*11) +....
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;test pi sum
(* 8 (pi-sum 1 1000))

;integral procedure
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;test
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;integral by simpson's rule
;http://en.wikipedia.org/wiki/Simpson%27s_Rule#Composite_Simpson.27s_rule
;we use the alternate non-expanded form
(define (simpsons-integral f a b n)
  (define (simh c d m)
    (/ (- d c) m))
  (define (simsum1 g x y m h)
    (define (simterm1 x)
      (+ x (* 2.0 h)))
    (* 2 (sum g (+ x (* 2.0 h)) simterm1 (+ x (* (- n 2.0) h)))))
  (define (simsum2 g x y m h)
    (define (simterm1 x)
      (+ x (* 2.0 h)))
    (* 4 (sum g (+ x h) simterm1 (+ x (* (- n 2.0) h)))))
  (* (/ (simh a b n) 3)
     (+ (f a)
        (simsum1 f a b n (simh a b n))
        (simsum2 f a b n (simh a b n))
        (f (+ a (* n (simh a b n)))))))

;test simpson's integral
(simpsons-integral cube 0 1 100)
(simpsons-integral cube 0 1 1000)
(simpsons-integral cube 0 1 10000)
(simpsons-integral cube 0 1 1000000)


;the sum procedure
(define (product term a next b)
  (accumulate * 1 term a next b))

;factorial interms of product

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

;an iterative accumulate
(define (accumulate-iter combine null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combine (term a) result))))
  (iter a null-value))

;the sum procedure
(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

;sum of cubes
(define (sum-cubes-i a b)
  (sum-iter cube a inc b))

;test
(sum-cubes-i 1 10)

;sum integers
(define (sum-integers-i a b)
  (sum-iter identity a inc b))

;test
(sum-integers-i 1 10)
