
(define (continue-primes n count)
  (cond ((= count 0)
         (display "are primes."))
        ((prime? n)
         (display n)
         (newline)
         (continue-primes (next-odd n) (- count 1)))
        (else
         (continue-primes (next-odd n) count))))


(define (test-foo)
  (let ((start-time (real-time)))
    (foo)
    (- (real-time) start-time)))


(define (search-for-primes n)
  (let ((start-time (real-time)))
    (continue-primes n 3)
    (- (real-time start-time))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else
         (find-divisor n (next test-divisor)))))



(define (search-for-primes n)
  (let ((start-time (real-time)))
    (continue-primes n 3)
    (- (real-time) start-time)))


