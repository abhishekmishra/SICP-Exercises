#lang scheme

(define tolerance 0.00001)

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

;(fixed-point cos 1.0)

; solution of x^x = 1000
;
; -> xlogx = log1000
; -> x = log1000/logx

(define fp (fixed-point
            (lambda (x) (/ (log 1000) (log x)))
            2.0))
(newline)
(display fp)
(newline)
(expt fp fp)