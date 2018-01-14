




 (define (iterative-improve good-enough? improve) 
   (lambda (x) 
     (define (iter n) 
       (if (good-enough? n) 
           n 
           (iter (improve n)))) 
     (iter x))) 
  
  
 (define (close-enough? v1 v2) 
   (< (abs (- v1 v2)) tolerance)) 
  
 (define (fixed-point f first-guess) 
   ((iterative-improve 
     (lambda (x) (close-enough? x (f x))) 
     (lambda (x) (f x))) 
    first-guess)) 
  
 (define (sqrt x) 
   ((iterative-improve 
     (lambda (y) 
       (< (abs (- (square y) x)) 
          0.0001)) 
     (lambda (y) 
       (average y (/ x y)))) 
    1.0)) 
  
