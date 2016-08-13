(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))

;exercise 2.1
(define (better-make-rat n d)
  (let ((g (gcd n d)))
    (if (or (and (< n 0) (< d 0)) (and (> n 0) (< d 0)))
        (cons (/ (* n -1) g) (/ (* d -1) g)))
        (cons (/ n g) (/ d g))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;exercise 2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
              (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

;exercise 2.3
(define (make-rectangle a b)
  (cons a b))
(define (length-rectangle r)
  (car r))
(define (height-rectangle r)
  (cdr r))
(define (perimeter-rectangle r)
  (* 2 (+ (length-rectangle r) (height-rectangle r))))
(define (area-rectangle r)
  (* (length-rectangle r) (height-rectangle r)))

(define (alt-cons x y)
  (lambda (m) (m x y)))
(define (alt-car z)
  (z (lambda (p q) p)))

;exercise 2.4
(define (alt-cdr z)
  (z (lambda (p q) q)))

;exercise 2.5
(define (make-madness a b)
  (* (expt 2 a) (expt 3 b)))
(define (extract-exponent x base result)
  (if (= (mod x base) 0)
      (extract-exponent (/ x base) base (+ 1 result))
      result))
(define (mad-car x)
  (extract-exponent x 2 0))
(define (mad-cdr x)
  (extract-exponent x 3 0))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;exercise 2.6
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (plus m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

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

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))

;exercise 2.7
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))

;exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;exercise 2.9
(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0))

;exercise 2.10
(define (div-interval-upg x y)
  (if (> (* (lower-bound y) (upper-bound y)) 0)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))
      (display 'Error!)))

;exercise 2.11
(define (mul-interval-ben x y)
    (cond ((and (< (lower-bound x) 0) (< (upper-bound x) 0) (< (lower-bound y) 0) (< (upper-bound y) 0))
           (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
          ((and (< (lower-bound x) 0) (< (upper-bound x) 0) (< (lower-bound y) 0))
           (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
          ((and (< (lower-bound x) 0) (< (upper-bound x) 0))
           (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
          ((and (< (lower-bound x) 0) (< (lower-bound y) 0) (< (upper-bound y) 0))
           (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
          ((and (< (lower-bound x) 0) (< (lower-bound y) 0))
           (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                          (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))
          ((< (lower-bound x) 0)
           (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
          ((and (< (lower-bound y) 0) (< (upper-bound y) 0))
           (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
          ((< (lower-bound y) 0)
           (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
          (else (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;exercise 2.12
(define (make-center-percent center percent)
  (let ((tolerance (/ percent 100.0)))
  (make-center-width center (* center tolerance))))
(define (percent interval)
  (* 100 (/ (- (upper-bound interval) (lower-bound interval) 2.0))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

;exercise 2.18
(define (reverse l)
  (define (rev-inner l res)
    (if (null? l)
        res
        (rev-inner (cdr l) (cons (car l) res))))
  (rev-inner l '()))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;exercise 2.19
(define (first-denomination l)
  (car l))
(define (except-first-denomination l)
  (cdr l))
(define (no-more? l)
  (null? l))

;exercise 2.20
(define (same-parity x . y)
  (define (inner-sp x y)
    (if (null? y)
        y
        (if (= (modulo (car y) 2) (modulo x 2))
            (cons (car y) (inner-sp x (cdr y)))
            (inner-sp x (cdr y)))))
  (inner-sp x y))

;exerice 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))
(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

;exercise 2.23
(define (for-each proc lst)
  (if (null? lst)
      (newline)
      (and (proc (car lst)) (for-each proc (cdr lst)))))

;exercise 2.25
;(car (cdr (car (cdr (cdr l)))))
;(car (car l))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))

;exercise 2.27
(define (deep-reverse lst)
  (define (dr-inner l res)
    (cond ((null? l) res)
          ((pair? (car l)) (dr-inner (cdr l) (cons (dr-inner (car l) '()) res)))
          (else (dr-inner (cdr l) (cons (car l) res)))))
  (dr-inner lst '()))
(define abc (list (list 1 2) (list 3 4)))
(define (dr-better lst)
  (if (pair? lst)
      (append (dr-better (cdr lst)) (list (dr-better (car lst))))
      lst))
(define (dr-best lst)
  (if (pair? lst)
      (reverse (map dr-best lst))
      lst))

;exercise 2.28
(define (fringe lst)
  (if (pair? lst)
      (every fringe lst)
      lst))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;exercise 2.29
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
(define (branch-weight branch)
    (if (number? (branch-structure branch))
        (branch-structure branch)
        (total-weight (branch-structure branch))))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))
(define (torque branch)
    (* (branch-length branch) (branch-weight branch)))
(define (balanced? mobile)
  (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
       (if (number? (branch-structure (left-branch mobile))) #t (balanced? left))
       (if (number? (branch-structure (right-branch mobile))) #t (balanced? right))))

(define (scale-tree1 tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree1 (car tree) factor)
                    (scale-tree1 (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;exercise 2.30
(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (* sub-tree sub-tree)))
       tree))
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree)
             (f sub-tree)))
       tree))

;exercise 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                            rest)))))

(define (accumulate2 op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate2 op initial (cdr sequence)))))

;exercise 2.33
(define (map-wtf p sequence)
  (accumulate2 (lambda (x y) (cons (p x) y)) '() sequence))
(define (append-wtf seq1 seq2)
  (accumulate2 cons seq2 seq1))
(define (length-wtf sequence)
  (accumulate2 (lambda (x y) (+ y 1)) 0 sequence))

;exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate2 (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
               0
               coefficient-sequence))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;exercise 2.35
(define (count-leaves2 atree)
  (accumulate2 (lambda (x y) (cond ((null? x) y)
                                   ((not (pair? x)) (+ y 1))
                                   (else (+ y (count-leaves2 x)))))
               0
               atree))
(define (count-leaves3 atree)
  (accumulate2 (lambda (x y) (+ x y))
               0
               (map (lambda (sub-tr) (if (pair? sub-tr)
                                         (count-leaves3 sub-tr)
                                         1)) atree)))

;exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate2 op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate2 + 0 (map * v w)))

(define amatrix '((1 2 3) (4 5 6) (7 8 9)))

;exercise 2.37
(define (matrix-*-vector-bad m v)
  (if (empty? m)
      '()
      (cons (dot-product (car m) v) (matrix-*-vector-bad (cdr m) v))))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate2)

;exercise 2.38
;3/2
;1/6
;(1 (2 (3 ())))
;(((() 1) 2) 3)
;Commutativity

;exercise 2.39
(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (flatmap proc seq)
  (accumulate2 append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list '())                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))
(define (prime-sum-pairs-better n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;exercise 2.41
(define (unique-triples n)
  (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                                             (enumerate-interval 1 (- j 1))))
                            (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))
(define (ordered-triplets n s)
       (filter (lambda (x) (= (accumulate + x) s))
               (unique-triples n)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;exercise 2.42
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (append (list new-row) rest-of-queens))
(define (safe? k positions)
  (define (inner-safe tested poss)
    (if (empty? poss)
        #t
        (if (or (= tested (car poss))
                (= tested (+ (- k (length poss)) (car poss)))
                (= tested (- (car poss) (- k (length poss)))))
            #f
            (inner-safe tested (cdr poss)))))
  (inner-safe (car positions) (cdr positions)))

;exercise 2.43
;It is O(n^n) instead of O(n^2), so it will execute in T^(n-2)

;section 2.2.4 in "project2.scm"

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;exercise 2.53
;'(a b c)
;'((george))
;'((y1 y2))
;'(y1 y2)
;#f
;#f
;#t

;exercise 2.54
(define (equal? x y)
  (cond ((or (null? x) (null? y)) (if (and (null? x) (null? y))
                                      #t
                                      #f))
        ((and (list? x) (list? y)) (if (equal? (car x) (car y))
                                       (equal? (cdr x) (cdr y))
                                       #f))
        (else (eq? x y))))

;exercise 2.55
;car of a list (quote abracadabra) yields quote.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

