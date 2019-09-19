#lang sicp
#| ====== operations ======= |#
(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (smallest-divisor n) (find-divisor n 2))
(define (square x) (* x x))
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))
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

;; exercise 2.40
(verbose-exercise "2.40")
(define (enumerate-interval lo hi)
  (if (> lo hi)
      nil
      (cons lo (enumerate-interval (+ lo 1) hi))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (unique-pairs n)
  (flatmap (lambda (j)
             (map (lambda (i)
                    (list i j))
                  (enumerate-interval 1 (- j 1))))
           (enumerate-interval 1 n)))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(unique-pairs 5)
(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))
(prime-sum-pairs 5)

;; exercise 2.41
(verbose-exercise 2.41)
(define (unique-pairs-2 n)
  (flatmap (lambda (y)
             (map (lambda (x) (list x y))
                  (enumerate-interval 1 y)))
           (enumerate-interval 1 n)))
(define (unique-triples n)
  (flatmap (lambda (z)
             (map (lambda (pair) (cons z pair))
                  (unique-pairs-2 z)))
           (enumerate-interval 1 n)))
(unique-triples 3)
(define (proc2-41 n s)
  (filter (lambda (triple)
            (= (accumulate + 0 triple) s))
          (unique-triples n)))
(proc2-41 3 6)

;; exercise 2.42
(verbose-exercise 2.42)
(define empty-board nil)
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list k new-row))))
(define (list-ref n seq)
  (cond ((null? seq) error "out of bound")
        ((= n 0) (car seq))
        (else (list-ref (- n 1) (cdr seq)))))
(define (safe? k positions)
  (let ((last (list-ref (- k 1) positions)))
    (define (collide-with-last? pos)
      (cond ((= (car pos) (car last)) #f)
            ((= (abs (- (car pos) (car last)))
                (abs (- (cadr pos) (cadr last))))
             #t)
            ((= (cadr pos) (cadr last)) #t)
            (else #f)))
    (null? (filter collide-with-last? positions))))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(queens 6)
#| ====== tests ======= |#
