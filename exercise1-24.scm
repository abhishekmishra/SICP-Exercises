#lang scheme
;section on primes

;fast-prime
;using Fermat's little theorem
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (myrandom x)
;  (display (round (* x (random))))
;  (newline)
  (round (* x (random))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (myrandom (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;prime? using fast-prime
(define (prime? n)
  (fast-prime? n 100))

;check order of growth through a timed test
(define (timed-prime-test n)
  (newline)
  (display n)
;  (newline)
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

(search-for-primes 1000 1020)
(search-for-primes 10000 10200)
(search-for-primes 100000 102000)
(search-for-primes 1000000 1020000)
(newline)
(sqrt 10)
;(search-for-primes 1000000000 1000000025)
;(search-for-primes 10000000000 10000000065)
;(search-for-primes 100000000000 100000000060)
;(search-for-primes 1000000000000 1000000000065)

;results of the last 3 are 2.5s, 1.4s, 0.45s which are close to an sqrt(10) multiples...
;REVISIT for result