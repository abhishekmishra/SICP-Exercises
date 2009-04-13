#lang scheme
;make-rat which handles both positive and negative arguments
(define (make-rat n d)
  (cond ((and (< n 0) (< d 0)) (make-rat (- n) (- d)))
        ((and (> n 0) (< d 0)) (make-rat (- (abs n)) (abs d)))
        (else (let ((g (gcd n d)))
                (cons (/ n g) (/ d g))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define r1 (make-rat -2 -4))
(print-rat r1)
(define r2 (make-rat 2 -4))
(print-rat r2)
(define r3 (make-rat -2 4))
(print-rat r3)
(define r4 (make-rat 2 4))
(print-rat r4)