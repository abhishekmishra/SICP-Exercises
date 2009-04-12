#lang scheme
;section on primes

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

;check order of growth through a timed test
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-process-milliseconds)))

(define (start-prime-test n start-time)
  (cond 
    ((prime? n) (report-prime (- (current-process-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((<= a b) 
         (cond ((odd? a) (timed-prime-test a)))
         (search-for-primes (+ a 1) b))))

;(search-for-primes 1000 1020)
;(search-for-primes 10000 10200)
;(search-for-primes 100000 102000)
;(search-for-primes 1000000 1020000)
(sqrt 10)
(search-for-primes 1000000000 1000000025)
(search-for-primes 10000000000 10000000065)
(search-for-primes 100000000000 100000000060)
(search-for-primes 1000000000000 1000000000065)

;results of the last 3 are 2.5s, 1.4s, 0.45s which are close to an sqrt(10) multiples...
;REVISIT for result