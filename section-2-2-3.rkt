#lang sicp
#| ====== operations ======= |#
(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

#| ====== exercises ====== |#
(define (verbose-exercise number)
  (newline)
  (display "exercise ")
  (display number)
  (newline))

;; exercise 2.33
(verbose-exercise 2.33)
(define (map proc sequence)
  (accumulate (lambda (curr-item rest-acc) (cons (proc curr-item) rest-acc)) nil sequence))
(map (lambda (x) (* x x)) (list 1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 2 3) (list 4 5 6))

(define (length sequence)
  (accumulate (lambda (curr-item rest-acc) (+ 1 rest-acc)) 0 sequence))
(length (list 1 2 3 4))
(length (list 1 (list 2 3 4)))

;; exercise 2.34
(verbose-exercise 2.34)
(define (horner-eval x coeff)
  (accumulate (lambda (curr-item rest-acc) (+ curr-item (* x rest-acc))) 0 coeff))
(horner-eval 2 (list 1 3 0 5 0 1))

;; exercise 2.35
(verbose-exercise 2.35)
(define (count-leaves t)
  (accumulate (lambda (curr-item rest-acc)
                (if (pair? curr-item)
                    (+ (count-leaves curr-item) rest-acc)
                    (+ 1 rest-acc)))
              0
              t))
(count-leaves (list 1 2 (list 3 4) (list 5 6 7)))

;; exercise 2.36
(verbose-exercise 2.36)
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;; exercise 2.37
(verbose-exercise 2.37)
(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))
(dot-product (list 1 2 3) (list 4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (row) (list (dot-product row v))) m))
(define matrix (list (list 1 2 3) (list 4 5 6)))
(define vector (list 10 10 10))
(matrix-*-vector matrix vector)

(define (transpose mat)
  (accumulate-n cons nil mat))
(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))
(define matrix2 (list (list 1 2) (list 1 2) (list 1 2)))
(matrix-*-matrix matrix matrix2)

;; exercise 2.38
(verbose-exercise 2.38)
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
(display "The constraint property should be that, for any two arguments") (newline)
(display "to be applied to a procedure, exchange the positions of the two") (newline)
(display "arguments gives the same result.") (newline)
(display "According to this property, +, * should gives the same result") (newline)
(display "regardless of fold-left/fold-right is used.") (newline)
(fold-left * 1 (list 1 2 3 4))
(fold-right * 1 (list 1 2 3 4))
(fold-left + 0 (list 1 2 3 4))
(fold-right + 0 (list 1 2 3 4))

;; exercise 2.39
(verbose-exercise 2.39)
(define (reverse sequence)
  (fold-right (lambda (curr-item rest-result) (append rest-result (list curr-item))) nil sequence))
(define (reverse-2 sequence)
  (fold-left (lambda (curr-item rest-result) (cons rest-result curr-item)) nil sequence))

(reverse (list 1 2 3))
(reverse-2 (list 1 2 3))
#| ====== tests ======= |#
