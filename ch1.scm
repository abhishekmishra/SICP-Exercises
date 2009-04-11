(define (square x)
  (* x x))

;;; Begin: exercise #1.3
;;; function returns the sum of squares of the 
;;; 2 bigger of the 3 numbers.
(define (square-of-max a b c)
  (cond
    ((and (< a b) (< a c)) (+ (square b) (square c)))
    ((and (< b a) (< b c)) (+ (square a) (square c)))
    ((and (< c a) (< c b)) (+ (square b) (square a)))))

(square-of-max 10 11 12)
;;; End: exercise #1.3

;;; Begin: exercise #1.4
;;; Test whether interpreter uses
;;; normal or applicative order evaluation.
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;commented to break - 
;(test 0 (p))
;;; End: exercise #1.4

;;; Begin: example #1.1.7 and exercise #1.6
;;; sqrt by guess. and a new if clause built as a procedure using a cond
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
;  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 10)

(sqrt 144)

;;; End: example #1.1.7 and exercise #1.6

;;; Begin: exercise #1.7
;;; to demonstrate that the good-enough? implementation above
;;; does not work well for very small or large numbers.
;;; and to create an implementation that does.

(define (good-enough-new? guess x)
  (< (- x (square guess)) (/ x 10000000000000)))

(define (sqrt-iter-new guess x)
  (if (good-enough-new? guess x)
      guess
      (sqrt-iter-new (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter-new 1.0 x))


(sqrt 0.001)

(sqrt 10e10)

;;; End: exercise #1.7

;;; Begin: exercise #1.8
;;; cube root by newton-raphson's method.
;;; guess = (/ (+ (/ x (square y)) (* 2 y)) 3)

(define (good-enough-cb? guess x)
  (< (abs (- (* (square guess) guess) x)) 0.001))

(define (improve-cb guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
  (if (good-enough-cb? guess x)
      guess
      (cube-root-iter (improve-cb guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(cube-root 27)

(cube-root 64)

(cube-root 1728)
;;; End: exercise #1.8

;;; Begin: section # 1.2.1
;;; Linear recursion and iteration

; recursive process for factorial calculation
(define (rfactorial n)
  (if (= n 1)
      1
      (* n (rfactorial (- n 1)))))

; iterative process for factorial calculation
(define (ifactorial n)
  (fact-iter 1 1 n))
  
(define (fact-iter counter product max-count)
  (if (> counter max-count)
      product
      (fact-iter
       (+ counter 1)
       (* product counter)
       max-count)))

(rfactorial 6)

(ifactorial 6)
;;; End: section # 1.2.1