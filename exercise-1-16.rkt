#lang sicp
(define (fast-expt n base) (fast-expt-iter n base 1))

(define (fast-expt-iter n base result)
  (cond ((= n 0) result)
        ((even? n) (fast-expt-iter (/ n 2) (square base) result))
        (else (fast-expt-iter (/ (- n 1) 2) (square base) (* base result)))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(fast-expt 5 2)

; The solution is very intersting. My solution starts from analysing the 01 patterns
; of the given N. By writing N in binary form, I realized that, base will always square
; as we move from right to left in the binary form of N. While the result accumulate
; current base's value.
;  n   base    result
; 111    b        1      <- last digit is 1, so we multiply result by base and pass that as argument
;  11    b^2      b      <- last digit is 1, so we multiply result by base and pass that as argument
;   1    b^4     b^3     <- last digit is 1, so we multiply result by base and pass that as argument
;   0    b^8     b^7     <- n = 0, return result

#|
The soluiton in the book is also inspiring. And it can actually leads to my solution.

(Hint: Using the observation that (bn/2)2 = (b2)n/2, keep, along with the exponent n and the base b,
an additional state variable a, and define the state transformation in such a way that the product
abn is unchanged from state to state. At the beginning of the process a is taken to be 1, and the answer
is given by the value of a at the end of the process. In general, the technique of defining an invariant
quantity that remains unchanged from state to state is a powerful way to think about the design of
iterative algorithms.)

|#