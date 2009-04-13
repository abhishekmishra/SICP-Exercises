#lang scheme

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

(define (square x) (* x x))

(define (length-segment s)
  (sqrt (+
         (square (- (x-point (end-segment s))
                    (x-point (start-segment s))))
         (square (- (y-point (end-segment s))
                    (y-point (start-segment s)))))))

;;TWO Representations of Rectangles
;;Uncomment one to use

;TYPE-1
;rectangle using points only
;points must be diagonally opposite
(define (make-rectangle x1 y1 x2 y2)
  (cons (make-point x1 y1) (make-point x2 y2)))

(define (height-rectangle r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (abs (- (y-point p2) (y-point p1)))))

(define (width-rectangle r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (abs (- (x-point p2) (x-point p1)))))


;TYPE-2
;rectangle using 2 adjacent side segments
#|
(define (make-rectangle x0 y0 x1 y1 x2 y2)
  (cons (make-segment (make-point x0 y0) (make-point x1 y1))
        (make-segment (make-point x0 y0) (make-point x2 y2))))

(define (height-rectangle r)
  (length-segment (car r)))

(define (width-rectangle r)
  (length-segment (cdr r)))
|#

;area and perimeter 
(define (area-rectangle r)
  (* (height-rectangle r)
     (width-rectangle r)))

(define (perimeter-rectangle r)
  (+ (* 2 (height-rectangle r))
     (* 2 (width-rectangle r))))

;test
;Type 1
(define r (make-rectangle 1 1 4 4)) 

;Type 2
;(define r (make-rectangle 1 1 1 4 4 1)) 

(display (area-rectangle r))
(display " square units.")
(newline)
(display (perimeter-rectangle r))
(display " units.")
