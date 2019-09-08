#lang sicp
#| ====== interval arthmetic operations ====== |#
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
(define (div-interval x y)                         ;exercise 2.11
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division error (interval spans 0)" y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))
(define (sub-interval x y)                         ;exercise 2.8
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]")
  (newline))

#| ====== interval number implementation ====== |#
;exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

#|
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center x)
  (/ (+ (upper-bound x) (lower-bound x)) 2))
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
|#

;exercise 2.12
(define (make-center-percent center percent)
  (let ((width (* center percent)))
    (make-interval (- center width) (+ center width))))
(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))
(define (percent x)
  (let ((width (/ (- (upper-bound x) (lower-bound x)) 2.0)))
    (/ width (center x))))
(define (print-interval-2 x)
  (display (center x))
  (display " +/- ")
  (display (* 100 (percent x)))
  (display "%")
  (newline))

#| ==== test ==== |#
(define x (make-interval 1 2))
(define y (make-interval 3 5))
(define z (make-interval -1 1))
(print-interval (add-interval x y))
(define a (make-center-percent 10 0.1))
(define b (make-center-percent 10 0.1))
(print-interval-2 a)
(print-interval-2 (add-interval a b))
