#lang scheme

(define tolerance 0.00001)

;modified to display guesses
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;newton's method
;---------------

;dx
(define dx 0.00001)

;derivative
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;test derivative
(define (cube x) (* x x x))

((deriv cube) 5)

;newton transform
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;cubic
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (* x x))
       (* b x)
       c)))

(define t1 (newtons-method (cubic 1 1 1) 1))
(display t1)
(newline)
((cubic 1 1 1) t1)

(define t2 (newtons-method (cubic 2 2 2) 1))
(display t2)
(newline)
((cubic 2 2 2) t2)

