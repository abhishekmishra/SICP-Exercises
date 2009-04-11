;pascal's triangle

; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1

(define (next-pascal-line current-line)
  (define (loop l1 l2)
    (if (eq? (cdr l2) ())
        (cons '1 l1)
        (loop (cons (+ (car l2) (cadr l2)) l1) (cdr l2))))
  (loop '(1) current-line))

(define (pascal n)
  (define (loop triangle m)
    (write (car triangle))
    (newline)
    (if (eq? m 0)
        #f;can return triangle too
        (loop (cons (next-pascal-line (car triangle)) triangle) (- m 1))))
  (loop '((1)) (- n 1)))
    
(pascal 15)