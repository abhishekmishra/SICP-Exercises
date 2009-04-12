#lang scheme

;evaluate k-term finite continued fraction
(define (cont-frac n d k)
  (define (cf n d k l)
    (if (> l k)
        0.0
        (/ (n l) (+ (d l) (cf n d k (+ l 1))))))
  (cf n d k 1))

(define (cont-frac-iter n d k)
  (define (cfi m e l result)
    (if (= l 0)
        result
        (cfi m e (- l 1) (/ (m l) (+ (e l) result)))))
  (cfi n d k 0.0))


(define (tan-cf x k)
  (define (tan-d x1)
    (- (* 2 x1) 1))
  (define (tan-n x1)
    (if (= x1 1) 
        x
        (- 0 (* x x))))
  (cont-frac tan-n tan-d k))

(tan-cf 1 1000)
(tan 1)

(tan-cf 0.5 1000)
(tan 0.5)
