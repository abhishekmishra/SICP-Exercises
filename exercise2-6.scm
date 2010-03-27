#lang scheme

;Computation with Church numerals
;see http://en.wikipedia.org/wiki/Church_encoding
;
;Some excellent notes at http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/doc/notes/church.txt


(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n1 n2)
  (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))

;multiplication is     mult -> λm.λn.λf. n (m f) 
(define (mult n1 n2)
  (lambda (f) (lambda (x) ((n1 (n2 f)) x))))

;exponent or power
(define (exp n1 n2)
  (n2 n1))

;test code
(define (to-integer n)
  (define (inc x) (+ 1 x))
  ((n inc) 0))

(to-integer (add-1 zero))

(to-integer (add-1 (add-1 zero)))

(to-integer (add-1 (add-1 one)))

(to-integer (add-1 (add-1 two)))

(to-integer (add one two))

(to-integer (mult (add one two) (add two two)))

(to-integer (exp (add one two) (add two two)))
