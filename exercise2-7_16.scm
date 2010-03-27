#lang scheme

;Section 2.1.4 - Extended Exercise: Interval Arithmetic
;Exercises 2.7 to 2.16

;-----------Exercise 2.7-------------
;Create the interval abstraction

(define (make-interval a b) (cons a b))

(define (lower-bound x) 
  (min (car x) (cdr x)))

(define (upper-bound x) 
  (max (car x) (cdr x)))

;test code
(define x (make-interval 1 3))
(define y (make-interval 2 5))
(display x)
(newline)
(display y)
(newline)
(display (lower-bound x))
(newline)
(display (upper-bound x))
(newline)

;Alyssa P. Hacker's code
;for addition, multiplication and division of intervals.

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;test code
(display (add-interval x y))
(newline)
(display (mul-interval x y))
(newline)
(display (div-interval x y))
(newline)
;----------Exercise 2.8--------------
;