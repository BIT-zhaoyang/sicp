#lang sicp
#| ====== operations ======= |#
(define (variable? e) ; Is e a variable?
  (symbol? e))
(define (same-variable? v1 v2) ; Are v1 and v2 the same variable?
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? e) ; Is e a sum?
  (and (pair? e) (eq? (car e) '+)))
(define (addend e) ; Addend of the sum e
  (cadr e))
(define (augend e) ; Augend of the sum e
  (caddr e))
(define (make-sum a1 a2) ;Construct the sum of a1 and a2
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list '+ a1 a2))))
(define (product? e) ;Is e a product?
  (and (pair? e) (eq? (car e) '*)))
(define (multiplier e) ; Multiplier of the product e
  (cadr e))
(define (multiplicand e) ;Multiplicant of the product e
  (caddr e))
(define (make-product m1 m2) ;Construct the product of m1 and m2
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
;; How do we come up with the simplication in the constructor of make-sum and
;; make-product? I think a reliable analysis is, by looking at the non-simplest
;; result of (deriv '(* (* x y) (+ x 3)) 'x), we can find reducing the result is
;; a recursive taks. In order to reduce the whole expression, we need to reduce
;; each part of the expression. When it comes to the tiniest unit, such as (+ 1 0),
;; we know we have to come up with a way to compute this. Thus, it comes the idea
;; we can do the simplication inside the constructors.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

#| ====== tests ====== |#
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

#| ====== exercises ====== |#
(define (verbose-exercise number)
  (newline)
  (display "exercise ")
  (display number)
  (newline))

;; exercise 2.53
(verbose-exercise 2.53)
(list 'a 'b 'c)
;(a b c)
(list (list 'george))
;((george))
(cdr '((x1 x2) (y1 y2)))
;((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;(y1 y2)
(pair? (car '(a short list)))
;false
(memq 'red '((red shoes) (blue socks)))
;false
(memq 'red '(red shoes blue socks))
;(red shoes blue socks)