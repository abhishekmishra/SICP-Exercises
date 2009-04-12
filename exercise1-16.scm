#lang mzscheme
; Process for iterative exponentiation process that uses
; successive squaring and uses a logarithmic number of steps.

;this is a linear recursive process
; that requires o(n) steps and o(n) space.
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(write (expt 2 10))
(newline)

; linear iterative process
; requires o(n) steps and o(1) space

(define (exptl b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(write (exptl 2 10))
(newline)
(write (exptl 2 11))
(newline)

; fast eponentiation with squares
; recursive method. o(log(n))
(define (square a) (* a a))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(write (exptl 2 10))
(newline)
(write (exptl 2 11))
(newline)

;iterative method

(define (fast-expt-i b n)
  (fei b n 1))

(define (fei b counter product)
  (cond ((= counter 0) product)
	((even? counter) (fei (square b) (/ counter 2) product))
        (else (fei b (- counter 1) (* b product)))))

(write (exptl 2 10))
(newline)
(write (exptl 2 11))
(newline)
