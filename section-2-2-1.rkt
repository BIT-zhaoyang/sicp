#lang sicp
#| ==== list operations ==== |#
(define (list-ref lst n)
  (cond ((null? lst) (error "out of bound"))
        ((= n 0) (car lst))
        (else (list-ref (cdr lst) (- n 1)))))
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))
(define (append list1 list2)
  ;; "cons up" an answer list while cdring down a list
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;; exercise 2-17
(define (last-pair lst)
  (if (null? lst)
      (error "empty list")
      (list-ref lst (- (length lst) 1))))
;; exercise 2-18
(define (prepend item lst)
  (cons item lst))
(define (reverse lst)
  (define (helper list1 list2)
    (if (null? list1)
        list2
        (helper (cdr list1) (prepend (car list1) list2))))
  (helper lst nil))

(define (map items proc)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map (cdr items) proc))))
;; exercise 2-23
(define (for-each items proc)
  (define (helper items proc)
    (proc (car items))
    (for-each (cdr items) proc))
  (if (null? items)
      nil
      (helper items proc)))

#| ==== exercises ==== |#
;; exercise 2-19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))
(define (no-more? coin-values) (null? coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (first-denomination coin-values) (car coin-values))

;; exercise 2-20
(define (filter lst predicate)
  (cond ((null? lst) nil)
        ((predicate (car lst)) (cons (car lst) (filter (cdr lst) predicate)))
        (else (filter (cdr lst) predicate))))
(define (same-parity . w)
  (filter w (lambda (x) (= (remainder (+ x (car w)) 2) 0))))
(define (same-parity-2 . w)
  (define (helper ele lst)
    (cond ((null? lst) nil)
          ((= (remainder (+ ele (car lst)) 2) 0) (cons (car lst) (helper ele (cdr lst))))
          (else (helper ele (cdr lst)))))
  (helper (car w) w))

#| ==== tests ==== |#
(define squares (list 1 4 9 16 25))
(list-ref squares 3)
(last-pair squares)
(reverse squares)
(cc 100 us-coins)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 1)
(same-parity-2 1 2 3 4 5 6 7)
(same-parity-2 2 3 4 5 6 7)
(same-parity-2 2 3 4 5 6 7 1)
(for-each (list 1 2 3 4 5) display)