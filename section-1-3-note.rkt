#lang sicp
; sqrt 1.1.7
#|
This is an implementation very specific to compute sqrt. Notice the helper
function is sqrt-iter. Sure we can abstract this by making <improve> a parameter
and pass arguments in. Still, it's relatively low level enough to describe each
step the process need to take. Unless one is very familiar with Newton's method,
it's not easy to tell this implements that.
|#
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; sqrt 1.3.3
#|
This implementation has induced a much more general process, fixed-point.
And this implementation gives clear indication we are using fixed-point to
compute sqrt. One place that is not clear enough is that, why we created a
lambda function involving <average>, when we calls fixed-point? The purpose
here is to eliminate guess oscillation by using average-damp. However,
expertise or comment is required to indicate this.
|#
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; sqrt 1.3.4
#|
This implemenation is an improvement compared to 1.3.3. We explicitly showing
that we are doing average-damp. That makes it a bit clearer.
|#
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; cube root
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


