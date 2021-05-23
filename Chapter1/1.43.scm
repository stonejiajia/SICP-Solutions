






(define (repeated f n)
    (if (= n 1)
        f 
        (compose f (repeated f (- n 1)))))

(define (compose f g) (lambda (x) (f (g x))))



((repeated square 2) 5)


