
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 0)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define odds (list 1 3 5 7))

(append odds squares)

;; 2.17

(last-pair (list 23 72 149 34))

(define (last-pair items)
  (if (null? items)
        (error "No items")
        (= (car items) (cdr items))
           (car items)
        (else (last-pair (cdr items)))))


(define (last-pair items)
  (if (= (length (cdr items)) 1)
         (cdr items)
         (last-pair1 (cdr items))))

;; 2.18

(define (car-last items)
  (if (null? items)
      (car items)
      (cons (car items))))

(define (pop-last items)
  (if (<= (length items) 2)
      (car items)
      (cons (car items) (pop-last (cdr items)))))



(define (reverse items)
  (if (= (length items) 1)
      items
      (append (last-pair items) ())))

(define nil '())

(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items))
              (cons (car items) nil))))

(define nil '())

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))
