
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

(length x)

(count-leaves x)

(list x x)

;; 2.24
;; 2.25

(define a (list 1 2 (list 5 7) 9))

(cdr (car (cdr (cdr a))))

(define a (list (list 7)))

(car (car a))

;; 2.26

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y)

(cons x y)

(list x y)

;; 2.27

(define x (list (list 1 2) (list 3 4)))

(define nil '())

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

(reverse x)

(define (deep-reverse items)
  (if (pair? (car items))
      (reverse (map deep-reverse items))
      items))

;; 2.28

(define (fringe x)
  (cond ((null? x) nil)
        ((pair? x)
         (append (fringe (car x))
                 (fringe (cdr x))))
        (else (list x))))
(define my-tree (list 1 (list 2 (list 3 4) (list 5 6)) (list 7 (list 8))))


(define (fringe tree)
  (define nil '())

  (define (build-fringe x result)
    (cond ((null? x) result)
          ((not (pair? x)) (cons x result))
          (else (build-fringe (car x)
                              (build-fringe (cdr x) result)))))

  (build-fringe tree nil))
