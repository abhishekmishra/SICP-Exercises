#lang scheme

;evaluate k-term finite continued fraction
(define (cont-frac n d k)
  (define (cf n d k l)
    (if (> l k)
        0
        (/ (n l) (+ (d l) (cf n d k (+ l 1))))))
  (cf n d k 1))

(define (cont-frac-iter n d k)
  (define (cfi m e l result)
    (if (= l 0)
        result
        (cfi m e (- l 1) (/ (m l) (+ (e l) result)))))
  (cfi n d k 0))

;procedure too get the successive d[i] for the continued fraction
(define (ev-d x)
  (cond ((= x 2) 2)
        ((= (remainder (- x 2) 3) 0) (+ (* 2 (/ (- x 2) 3)) 2))
        (else 1)))

(define (testevd x l) 
  (cond ((> l x) #f)
        (else 
         (display (ev-d l))
;         (newline)
         (testevd x (+ l 1)))))

(testevd 20 0)

(define (e-value k)
  (+ 2 (cont-frac 
        (lambda (i) 1.0)
        ev-d
        k)))

(e-value 10)
(e-value 100)
(e-value 1000)
(e-value 10000)
;(e-value 10000000)