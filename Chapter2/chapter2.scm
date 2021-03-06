





(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom y))))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  ;(newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(print-rat one-half)


(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(define (gcd a b)
  (if (= b 0)
      a
  (gcd b (remainder a b))))

(make-rat 1 (- 2))

(define (make-rat n d)
  
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(make-rat -2 -8)

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))

(make-rat 2 -8)

(define (cons1 x y)
  (define (dispath m)
    (cond ((= m 0) m)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispath)

(define (car z) (z 0))

(define (cdr z) (z 1))

(car (1 2 3))

(car (cons 1 2))
