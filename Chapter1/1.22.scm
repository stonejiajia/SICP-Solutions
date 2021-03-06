
(define (square x) (* x x))

(define (next-odd n)
  (if (odd? n)
      (+ 2 n)
      (+ 1 n)))

(define (prime? n)
    (= n (smallest-divisor n)))


(define (smallest-divisor n)
    (find-divisor n 2))



(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
            n)
          ((divides? test-divisor n)
            test-divisor)
          (else
            (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
    (= (remainder b a) 0))


(define (continue-primes n count)
    (cond ((= count 0)
            (display "are primes."))
          ((prime? n)
            (display n)
            (newline)
            (continue-primes (next-odd n) (- count 1)))
          (else
            (continue-primes (next-odd n) count))))


(prime? 1999)

(continue-primes 100 2
                 )

(time (continue-primes 100 2
                       ))
