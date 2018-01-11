
(define (sq x) (* x x))



(define (smallest-div n) 
    (define (divides? a b) 
      (= 0 (remainder b a))) 
    (define (find-div n test) 
       (cond ((> (sq test) n) n) ((divides? test n) test) 
             (else (find-div n (+ test 1))))) 
    (find-div n 2)) 
  
(define (prime? n) 
  (if (= n 1) false (= n (smallest-div n)))) 







(define (filtered-accumulate combiner null-value term a next b filter) 
  (if (> a b) null-value 
      (if (filter a) 
          (combiner (term a) 
                    (filtered-accumulate combiner null-value term (next a) next b filter)) 
          (combiner null-value 
                    (filtered-accumulate combiner null-value term (next a) next b filter))))) 





