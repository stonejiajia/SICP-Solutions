






(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (i = 0)
        (/ (n i) (+ (d i) result))
        (cont-frac-iter (- i 1)
                        (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0.0))


;; x 弧度 n

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (- (* x x))))
             (lambda (i) 
               (- (* 2 i) 1))
             k))




(define (tan-cf x k) 
   (define (iter i result) 
     (if (= i 0) 
         result 
         (iter (-1+ i) 
               (/ (if (= i 1) x (square x)) 
                  (- (- (* 2 i) 1) 
                     result))))) 
   (iter k 0)) 


(define (tan-cf x k)
    
    (define (N i)
        (if (= i 1)
            x
            (- (square x))))

    (define (D i)
        (- (* i 2) 1))

    (exact->inexact (cont-frac N D k)))

(tan-cf 20 20)

(define (square x) (* x x))

(define (cont-frac-iter n d k)
  (define (iter k result)
    (cond ((zero? k) result)
          (else (iter (- k 1) 
                      (/ (n k)
                         (+ (d k) result))))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) 
                    (if (= i 1) x (- (square x))))
                  (lambda (i) 
                    (- (* 2 i) 1))
                  k))

(tan-cf 20 10)
