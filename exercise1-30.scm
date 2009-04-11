#lang scheme

;section on procedures as arguments

;iterative procedure for sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

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
