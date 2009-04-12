#lang scheme

;section on procedures as general methods.

(define (f g)
  (g 2))

(define (square x) (* x x))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)
;in the call above we have
; (f f) -> (f 2) -> (2 2)
; since 2 is not a procedure, the last call fails.
