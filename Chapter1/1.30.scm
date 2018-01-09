



(define (next k)
  (+ k 1))

(define (simpson f a b n)
    
    (define h (/ (- b a) n))

    (define (y k)
        (f (+ a (* k h))))

    (define (factor k)
        (cond ((or (= k 0) (= k n))
                1)
              ((odd? k)
                4)
              (else
                2)))
    
    (define (term k)
        (* (factor k)
           (y k)))

    (define (next k)
        (+ k 1))

    (if (not (even? n))
        (error "n can't be odd")
        (* (/ h 3)
           (sum term (exact->inexact 0) next n))))




(define (cube x)
          (* x x x))




(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ (term a) result))))
  (iter a 0))


(simpson cube 0 1 100)

(time (simpson cube 0 1 1000))

;;
;;
;;(time (simpson cube ...))
 ;   3 collections
 ;   0.041221035s elapsed cpu time, including 0.000150196s collecting
 ;   0.041512000s elapsed real time, including 0.000160000s collecting
 ;   24802192 bytes a
;;
;;
;;
