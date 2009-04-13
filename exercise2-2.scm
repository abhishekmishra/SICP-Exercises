#lang scheme
;representation of points and segments

;points
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;segments
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-segment s)
  (print-point (start-segment s))
  (print-point (end-segment s)))

(define (midpoint-segment s)
  (make-point 
   (/ (+ (x-point (start-segment s))
         (x-point (end-segment s)))
      2)
   (/ (+ (y-point (start-segment s))
         (y-point (end-segment s)))
      2)))

;test
(define s1 (make-segment (make-point 1 1) (make-point 5 5)))
(print-segment s1)
(print-point (midpoint-segment s1))