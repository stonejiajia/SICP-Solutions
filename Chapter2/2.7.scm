

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval1 x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (- x (* deviation x)))

(define (upper-bound x)
  (+ x (* deviation x)))

(define (upper-bound interval) (max (car interval) (cdr interval)))


(define (lower-bound interval) (min (car interval) (cdr interval)))

(define i (make-interval 2 7))
(upper-bound i)

; 2.8

(define j (make-interval 3 8))

(add-interval i j)

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y)) (- (lower-bound y)))))


;; 2.9

;; 2.10

(define k (make-interval 9 9))

(define (div-interval2 x y)
  (if (<= 0 (* (lower-bound y) (upper-bound y)))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (div-interval x y)
  (if (= (upper-bound y)
         (lower-bound y))
      (error "Division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; 2.11

(define (negative-interval? x) ( (lower-bound x) 0))

(define (mul-interval-signs x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (negative-interval? x)
                (negative-interval? y)) (make-interval (* ux uy) (* lx ly)))
          ((and (positive-interval? x)
                (positive-interval? y)) (make-interval (* lx ly) (* ux uy)))
          ((and (negative-interval? x)
                (positive-interval? y)) (make-interval (* lx uy) (* ux ly)))
          ((and (positive-interval? x)
                (negative-interval? y)) (make-interval (* ux ly) (* lx uy)))
          ((and (positive-interval? x)
                (spans-zero?   y)) (make-interval (* ux ly) (* ux uy)))
          ((and (negative-interval? x)
                (spans-zero?   y)) (make-interval (* lx uy) (* lx ly)))
          ((and (spans-zero?   x)
                (positive-interval? y)) (make-interval (* lx uy) (* ux uy)))
          ((and (spans-zero?   x)
                (negative-interval? y)) (make-interval (* ux ly) (* lx ly)))
          (else (make-interval (* (min lx ly) (max ux uy))
                               (max (* ux uy) (* lx ly)))))))

;; 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tolerance)
  (make-center-width c (* c (/ tolerance 100))))

(define (percent interval)
  (/ (* 100 (width interval))
     (center interval)))

;; 2.13

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
