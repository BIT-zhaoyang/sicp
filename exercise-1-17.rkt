#lang sicp
(define (* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (* (double a) (half b)))
        (else (+ a (* a (- b 1))))))

(define (even? x)
  (= (remainder x 2) 0))

(define (double x)
  (+ x x))     ; Don't use (* x 2) here! We are defining '*', it will recurse!

(define (half x)
  (/ x 2))

(* 7 7)

; exercise 1.18
(define (iter-multi a b extra)
  (cond ((= b 0) 0)
        ((= b 1) (+ a extra))
        ((even? b) (iter-multi (double a) (half b) extra))
        (else (iter-multi a (- b 1) (+ extra a)))))
(define (multi a b)
  (iter-multi a b 0))
(multi 9 6)