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

;----------Exercise 2.10---------------
;Modify Alyssa's code to check division by an interval that spans zero
(define (interval-spans-zero? x)
  (if (and (>= (upper-bound x) 0) (<= (lower-bound x) 0))
      (error "Division by an interval that spans zero!!")
      #t))

(define (div-interval x y)
  (interval-spans-zero? y)
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
;sub-interval using reasoning analogous to Alyssa's
;
;we subtract the upper bound and the lower bound of the second interval from the upper bound and the lower bound of the first interval respectively.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;test
(display (sub-interval x y))
(newline)

;----------Exercise 2.9---------------
;width of the interval is half of the difference between its upper an dlower bounds.
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(display (width-interval x))
(newline)
(display (width-interval y))
(newline)

;for some function of intervals
;width(f(interval1, interval2....)) is a function only of the widths of interval1, interval2...
;
;show this for subtraction and addition
; width [x, y] = y - x
; width [r, t] = t - r
;
; width ([x, y] + [r, t]) = width [x + r, y + t] = (y + t - x - r)/2 = (y - x + t - r)/2 = (width [x, y]) + (width [r, t])
; width ([x, y] - [r, t]) = width [x - t, y - r] = (y - r - x + t)/2 = (width [x, y]) + (width [r, t])
;
;and show examples to show this is not true for multiplication and division
(display (width-interval (add-interval x y)))
(newline)
(display (width-interval (sub-interval x y)))
(newline)
(display (width-interval (div-interval x y)))
(newline)
(display (width-interval (mul-interval x y)))
(newline)

;----------Exercise 2.10---------------
;Modify Alyssa's code to check division by an interval that spans zero
;code added earlier in the file.

;uncomment one by one to test
;(display (div-interval (make-interval 1 2) (make-interval -1 2)))
;(newline)
;(display (div-interval (make-interval 1 2) (make-interval 1 0)))
;(newline)

;----------Exercise 2.11---------------

(define (negative? num)
  (if (< num 0)
      #t
      #f))

(define (positive? num)
  (not (negative? num)))

(define (mul-interval-2 x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((or
            (and (negative? x1) (negative? y1) (positive? x2) (positive? y2)))
           (let ((p1 (* x1 y2))
                 (p2 (* x2 y1))
                 (p3 (* x2 y2))) 
             (make-interval (min p1 p2) p3)))
          ((or
            (and (positive? x1) (positive? y1) (positive? x2) (positive? y2))
            (and (negative? x1) (negative? y1) (negative? x2) (negative? y2))
            (and (positive? x1) (negative? y1) (positive? x2) (negative? y2))
            (and (negative? x1) (positive? y1) (negative? x2) (positive? y2)))
           (make-interval (* x1 y1) (* x2 y2)))
          ((or
            (and (negative? x1) (positive? y1) (positive? x2) (positive? y2))
            (and (negative? x1) (negative? y1) (positive? x2) (negative? y2)))
           (make-interval (* x1 y2) (* x2 y2)))
          ((or
            (and (positive? x1) (negative? y1) (positive? x2) (positive? y2))
            (and (negative? x1) (negative? y1) (negative? x2) (positive? y2)))
           (make-interval (* x2 y1) (* x2 y2))))))

(display (mul-interval x y))
(newline)
(display (mul-interval-2 x y))
(newline)

;Alyssa's alternate constructor and selectors
;intervals represented as a center value and an additive tolerance.
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;----------Exercise 2.12---------------
;constructor and selector percent for intervals represented as a center and a percentage tolerance
;selector for center remains the same as above
(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-center-width c w)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

;test
(display (make-center-width 10 2)) ;[8, 12]
(newline)
(display (center (make-center-width 10 2))) ;10
(newline)
(display (width (make-center-width 10 2))) ;2
(newline)

(display (make-center-percent 10 20)) ;[8, 12]
(newline)
(display (center (make-center-percent 10 20))) ;10
(newline)
(display (percent (make-center-percent 10 20))) ;20
(newline)
