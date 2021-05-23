


(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (log-reduce n base)
  (cond ((not (zero? (remainder n base))) 0)
        (else (+ (log-reduce (/ n base) base) 1))))

(define (car z)
  (log-reduce z 2))

(define (cdr z)
  (log-reduce z 3))

(define test-pair (cons 11 7))

(car test-pair)

(cdr test-pair)
