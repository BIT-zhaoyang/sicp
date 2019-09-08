#lang sicp
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; exercise 1.21
;(smallest-divisor 19999)

; exercise 1.22
(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime n (- (runtime) start-time))
      #f))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start number)
  (if (> number 0)
      (if (timed-prime-test start)
          (search-for-primes (+ start 1) (- number 1))
          (search-for-primes (+ start 1) number))))

(search-for-primes 1000 3)
;(search-for-primes 10000 3)
;(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 1000000000 3)


