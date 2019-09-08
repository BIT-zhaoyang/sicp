#lang sicp
#| ==== Tree Operations ==== |#
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

#| ==== Exercise ==== |#
(define (verbose-exercise number)
  (newline)
  (display "exercise ")
  (display number)
  (newline))

;; exercise 2.24
(verbose-exercise "2.24")
(list 1 (list 2 (list 3 4)))
;; exercise 2.25
(verbose-exercise "2.25")
(car (cdaddr (list 1 3 (list 5 7) 9)))
(caar (list (list 7)))
(car (cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7)))))))))))
(verbose-exercise "2.26")
;; exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)
;; exercise 2.27
(verbose-exercise "2.27")
(define (reverse x)
  (define (helper seq res)
    (cond ((null? seq) res)
          ((not (pair? seq)) seq)
          (else (helper (cdr seq) (cons (reverse (car seq)) res)))))
  (helper x nil))
(reverse (list x y))
;; exercise 2.28
(verbose-exercise "2.28")
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))
(define a (list (list 1 2) (list 3 4)))
(fringe a)
(fringe (list a a))

#| ==== Tests ==== |#
(define t (cons (list 1 2) (list 3 4)))
;(count-leaves t)