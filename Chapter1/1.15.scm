(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (abs x)
  (if (> x 0)
      x
      (- x)))

(sine (/ 3.14159265354 6))

(sine 12.15)


(trace sine)
(sine 12.15)

