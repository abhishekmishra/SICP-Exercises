;Exercise 1.15
;The sine of an angle(specified in radians) computed by approximation
;
; sin(x) ~ x for small values of x (x < .1 radians)
; and
; sin (x) = 3 * sin (x/3) - 4 * (sin (x/3))^3
; 
; the question is how many times p will be called for (sin 12.15)
; the answer is 4

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.5)