






(define (cont-frac n d k) 
   (define (frac-rec i) 
     (/ (n i) 
        (+ (d i) 
           (if (= i k) 
               0 
               (frac-rec (+ i 1)))))) 
   (frac-rec 1)) 



(define (golden-ratio k)
    (+ 1
       (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k)))

(trace cont-frac)

(golden-ratio 1)


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

((lambda (i) 3.0) 2000000)

(lambda (i) 3)

(define greet
  (case-lambda
    [(name) (string-append "Hello, " name)]
    [(given surname) (string-append "Hello, " given " " surname)]))

(greet "John")

