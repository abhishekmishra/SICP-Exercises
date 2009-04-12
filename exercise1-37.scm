#lang scheme

;evaluate k-term finite continued fraction
(define (cont-frac n d k)
  (define (cf n d k l)
    (if (> l k)
        0
        (/ (n l) (+ (d l) (cf n d k (+ l 1))))))
  (cf n d k 1))

(define (phi k)
  (/ 1 
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))

(phi 10)
(phi 100)
(phi 1000)

(define (cont-frac-iter n d k)
  (define (cfi m e l result)
    (if (= l 0)
        result
        (cfi m e (- l 1) (/ (m l) (+ (e l) result)))))
  (cfi n d k 0))

(define (phi-i k)
  (/ 1 
     (cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))

(phi-i 10)
(phi-i 100)
(phi-i 1000)
