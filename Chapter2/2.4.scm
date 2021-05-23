


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

;; 代换模型

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(f 5)

((lambda (m) (m x y)) (lambda (p q) q))

(expt 2 9)

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


(define (cons1 x y)
  (lambda (m) (m x y)))

(define (car1 z)
  (z (lambda (p q) p)))

(define (cdr1 z)
  (z (lambda (p q) q)))
