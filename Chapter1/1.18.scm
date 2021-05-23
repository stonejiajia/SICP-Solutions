

(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (mult3 a b)
  (define (mult-iter accumulator b c)
    (cond ((= c 0) accumulator)
          ((even? c) (mult-iter accumulator (double b) (halve c)))
          (else (mult-iter (+ accumulator b) (double b) (- (halve c) 0.5)))))
  (mult-iter 0 a b))

(define (mult-iter accumulator b c)
    (cond ((= c 0) accumulator)
          ((even? c) (mult-iter accumulator (double b) (halve c)))
          (else (mult-iter (+ accumulator b) (double b) (- (halve c) 0.5)))))


(mult-iter 0 2 3)
(trace mult-iter)

