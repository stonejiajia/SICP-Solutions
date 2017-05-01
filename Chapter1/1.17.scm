

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (product a b)
  (cond ((= b 0) 0)
        ((even? b) (double (product a (halve b))))
        (else (+ a (product a (- b 1))))))


 
