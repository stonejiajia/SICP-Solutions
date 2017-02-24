(define (cub-iter guess x)
  (if (good-enough? guess x)
      guess
      (cub-iter (improve guess x) x)))

(define (square x) (* x x))

(define (improve guess x)
  (/
   (+
    (* 2 guess) (/ x (square guess))) 3))

(define (cube x) (* x (square x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cub x)
  (cub-iter 1.0 x))


