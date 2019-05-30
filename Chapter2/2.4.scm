


(cons 1 2)


(define (cons1 x y)
  (trace-lambda (m) (m x y)))

(cons1 1 2)

(define (car1 z)
  (z (trace-lambda (p q) p)))

(car1 (cons1 7 8 ))

((lambda x x)
 1 2 3)

((lambda x (car x))
 1 2 3)


(define max-mag
  (trace-lambda nums
    (apply max (map magnitude nums))))

(max 1 -2 0)


(max-mag 1 -2 0 8)


(define half
  (trace-lambda half (x)
    (cond
      [(zero? x) 0]
      [(odd? x) (half (- x 1))]
      [(even? x) (+ (half (- x 1)) 1)])))

(half 5)


;; lambda 用法

((lambda (x) x) 1)

((lambda (x y) (+ x y)) 1 2)

;; procedure
(lambda (m) (m x y))

(lambda (x) x)
