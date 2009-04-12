#lang scheme

;accumulate procedure with an additional filter argument.
;a recursive filtered-accumulate
(define (filtered-accumulate filter combine null-value term a next b)
  (if (> a b)
      null-value
      (combine 
       (if (filter a)
           (term a)
           null-value)
       (filtered-accumulate filter combine null-value term (next a) next b))))

;smallest divisor (improved to exclude even numbers)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;the sum procedure
(define (sum filter term a next b)
  (filtered-accumulate filter + 0 term a next b))

;increment procedure
(define (inc n) (+ n 1))

;sum of cubes
(define (sum-squares-prime a b)
  (sum prime? square a inc b))

;test
(sum-squares-prime 2 10)

(+
 (square 2)
 (square 3)
 (square 5)
 (square 7))

;the product procedure
(define (product filter term a next b)
  (filtered-accumulate filter * 1 term a next b))

;gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;identity
(define (identity x) x)

(define (product-relative-prime n)
  ;check relative prime
  (define (relative-prime? i)
    (if (= (gcd i n) 1)
        #t
        #f))
  (product relative-prime? identity 1 inc n))

(product-relative-prime 10)

(* 1 3 7 9)