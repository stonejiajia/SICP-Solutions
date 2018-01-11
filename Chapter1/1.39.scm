






(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (i = 0)
        (/ (n i) (+ (d i) result))
        (cont-frac-iter (- i 1)
                        (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0.0))


;; x 弧度 n

(define (tan-cf x)
  (cont-frac (lambda (i)
               (if (= (remainder i 2) 0)
                   (* x x)
                   x))
             (lambda (i) (- (* 2 i) 1))
             k))


