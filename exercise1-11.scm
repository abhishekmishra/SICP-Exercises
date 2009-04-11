;f(n) = n, if n < 3
;and,
;f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3), if n >=3

(define (f n)
 (cond ((< n 3) n)
       (else (+
              (f (- n 1))
              (* 2 (f (- n 2)))
              (* 3 (f (- n 3)))))))

(f 3)
(f 4)
(f 5)
(f 6)
(f 7)
(f 10)

(define (f-iter a b c count)
 (cond ((= count 0) 0)
       ((= count 1) 1)
       ((= count 2) 2)
       ((> count 2) (f
