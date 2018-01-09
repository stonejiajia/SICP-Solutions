

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))  ; p'
                   (+ (* 2 p q)  (square q))  ; q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;; 1.22

(define (fib2 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib2 (- n 1))
                 (fib2 (- n 2))))))




(define (fib3 n)
  (fib-iter3 1 0 n))

(define (fib-iter3 a b count)
  (if (= count 0)
      b
      (fib-iter3 (+ a b) a (- count 1))))


(trace fib-iter3)




