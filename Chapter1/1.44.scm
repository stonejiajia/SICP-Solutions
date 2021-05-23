
(define (square x) (* x x))

(define dx 0.00001)

(define (repeated f n)
    (if (= n 1)
        f 
        (compose f (repeated f (- n 1)))))

(define (compose f g) (lambda (x) (f (g x))))


(define (smooth f)
  (lambda (x)
    (/ (+ (f (+ x dx)) (f x) (f (- x dx))))))



(define (n-fold-smooth f n)
  ((repeated smooth n) f))

((n-fold-smooth square 3) 4.0)

