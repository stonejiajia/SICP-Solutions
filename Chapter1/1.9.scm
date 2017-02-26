(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))


(define (inc a) (+ a 1))
(define (dec a) (- a 1))


(define (+ a b)
  (if (= a b)
      b
      (inc (+ (dec a) b))))


(+ 2 3)
(inc (+ 1 3))
(inc (inc (+ 0 3)))
(inc (inc 3))
(inc 4)
(5)

(+ 2 3)
(+ 1 4)
(+ 0 5)
5












