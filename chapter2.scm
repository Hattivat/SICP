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

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;exercise 2.56
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
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp) '-1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** b e))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

;exercise 2.57
(define (not-number? x)
  (not (number? x)))
(define (is-zero? x)
  (=number? x 0))
(define (simplify-sum alist)
  (let ((num-sum (accumulate + (filter number? alist)))
        (not-num (filter not-number? alist)))
    (if (= 0 num-sum)
        not-num
        (cons num-sum not-num))))
(define (make-sum . args)
  (let ((result (simplify-sum args)))
    (if (= 1 (length result))
        (car result)
        (cons '+ result))))
(define (addend s) (cadr s))
(define (augend s) (if (null? (cdddr s))
                       (caddr s)
                       (cons '+ (cddr s))))
(define (simplify-prd alist)
  (let ((num-prd (accumulate * (filter number? alist)))
        (not-num (filter not-number? alist)))
    (cond ((= 0 num-prd) (list 0))
          ((= 1 num-prd) not-num)
          (else (cons num-prd not-num)))))
(define (make-product . args)
  (let ((result (simplify-prd args)))
    (if (= 1 (length result))
        (car result)
        (cons '* result))))
(define (multiplier p) (cadr p))
(define (multiplicand p) (if (null? (cdddr p))
                             (caddr p)
                             (cons '* (cddr p))))

;exercise 2.58
(define (make-sum2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (sum?2 x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product?2 x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier2 p) (car p))
(define (multiplicand2 p) (caddr p))
(define (addend2 s) (car s))
(define (augend2 s) (caddr p))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;exercise 2.59
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;exercise 2.60
;element-of-set? stays the same.
(define (adjoin-set2 x set)
  (cons x set))
(define (union-set2 set1 set2)
  (append set1 set2))
(define (remove-element set x)
  (if (= x (car set))
      (cdr set)
      (cons (car set) (remove-element (cdr set) x))))
(define (intersection-set2 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set2 (cdr set1) (remove-element (car set1) set2))))
        (else (intersection-set2 (cdr set1) set2))))

;ordered sets
(define (ord-element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (ord-element-of-set? x (cdr set)))))

(define (ord-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (ord-intersection-set (cdr set1)
                                           (cdr set2))))
              ((< x1 x2)
               (ord-intersection-set (cdr set1) set2))
              ((< x2 x1)
               (ord-intersection-set set1 (cdr set2)))))))

;exercise 2.61
(define (ord-adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((ord-element-of-set? x set) set)
        ((> x (car set)) (cons (car set) (ord-adjoin-set x (cdr set))))
        ((< x (car set)) (cons x set))))

;exercise 2.62
(define (ord-union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (ord-union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (ord-union-set (cdr set1) set2)))
                      ((> x1 x2) (cons x2 (ord-union-set set1 (cdr set2)))))))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tr-element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (tr-element-of-set? x (left-branch set)))
        ((> x (entry set))
         (tr-element-of-set? x (right-branch set)))))

(define (tr-adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (tr-adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (tr-adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;exercise 2.63
;The two procedures produce the same results.
;The first procedure is O(n*logn), the second is O(n)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;exercise 2.64
;This procedure works by first taking the first half minus one (for entry point) of the given nr of
;elements, and recursively calling itself on that set of elements to construct the left branch of the
;tree, then saving the median element to be the tree's entry point, and again calling itself recursively
;on the rest of the elements to create the right branch of the tree. Once it has both branches, it calls
;make-tree providing it with the entry element it found and the branches it has constructed. Finally, it
;creates and returns a pair with the constructed list as it first element, and the remainign elements
;(or empty list if there are none) as the other.
;The tree resulting from the example ordered list would be:
;  5
; / \
;1   9
; \  /\
; 3 7 11
;The order of growth for this procedure is O(n).

;exercise 2.65
(define (tr-union-set set1 set2)
  (let ((ordset1 (tree->list-2 set1))
        (ordset2 (tree->list-2 set2)))
    (let ((ordset (ord-union-set ordset1 ordset2)))
      (list->tree ordset))))
(define (tr-intersection-set set1 set2)
  (let ((ordset1 (tree->list-2 set1))
        (ordset2 (tree->list-2 set2)))
    (let ((ordset (ord-intersection-set ordset1 ordset2)))
      (list->tree ordset))))

;exercise 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((tested-key (car (entry set-of-records))))
        (cond ((= given-key tested-key) (entry set-of-records))
              ((< given-key tested-key)
               (lookup given-key (left-branch set-of-records)))
              ((> given-key tested-key)
               (lookup given-key (right-branch set-of-records)))))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (htr-adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (htr-adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (htr-adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;exercise 2.67
; '(A D A B B C A)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;exercise 2.68
(define (encode-symbol symbol tree)
  (define (inner-encode symbol tree)
    (cond ((leaf? tree)
           '())
          ((element-of-set? symbol (symbols (left-branch tree)))
           (cons 0 (inner-encode symbol (left-branch tree))))
          ((element-of-set? symbol (symbols (right-branch tree)))
           (cons 1 (inner-encode symbol (right-branch tree))))))
  (if (element-of-set? symbol (symbols tree))
      (inner-encode symbol tree)
      (error "Symbol not found! -- encode-symbol" symbol)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;exercise 2.69
(define (successive-merge leafset)
  (if (= (length leafset) 1)
      (car leafset)
      (successive-merge (htr-adjoin-set (make-code-tree (car leafset) (cadr leafset))
                                    (cddr leafset)))))

;exercise 2.70
;the theoretical minimum for variable-length encoding is 84 bits.
;for fixed-length encoding the minimum is 108 bits.

;exercise 2.71
;In such a tree the most frequent symbol requires 1 bit, while the least frequent symbol requires n-1 bits.

;exercise 2.72
;The most common symbol is O(n), the least common one is O(n2).

;section 2.4

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;;;;;;;;;;;;;;;;;;;

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;exercise 2.73
;A)
;We can't assimilate the predicates number? and same-variable? into the data-directed dispatch because
;of scoping - the information on whether a variable or a number was passed will be lost when it is passed
;further down the chain of functions.
;B)
(define (install-sum-package)
  (define (augend asum) (car asum))
  (define (addend asum) (cdr asum))
  (define (make-sum x y)
    (cond ((and (number? x) (number? y)) (+ x y))
          ((=number? x 0) y)
          ((=number? y 0) x)
          (else (list 'sum x y))))
  (define (sum-deriv exp var)
    (make-sum (deriv (augend exp) var)
              (deriv (addend exp) var)))
  (put 'deriv 'sum sum-deriv))
(define (install-mult-package)
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cdr exp))
  (define (make-product x y)
    (cond ((and (number? x) (number? y)) (* x y))
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((or (=number? x 0) (=number? y 0)) 0)
          (else (list 'prod x y))))
  (define (mult-deriv exp var)
    (make-sum (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))))
  (put 'deriv 'prod mult-deriv))
;C)
(define (install-div-package)
  (define (dividend exp) (car exp))
  (define (divisor exp) (cdr exp))
  (define (make-quotient x y)
    (cond ((and (number? x) (number? y)) (/ x y))
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((=number? x 0) 0)
          ((=number? y 0) (error "cannot divide by zero!" y))
          (else (list 'quot x y))))
  (define (div-deriv exp var)
    (make-quotient (make-sum (make-product (deriv (dividend exp) var)
                                           (divisor exp))
                             (make-product -1 (make-product (dividend exp)
                                                            (deriv (divisor exp) var))))
                   (make-product (divisor exp) (divisor exp))))
  (put 'deriv 'quot div-deriv))
;D)
;The order of arguments passed to put would need to be changed.

;exercise 2.74
;A)
;The record can be structured in any way, as long as all of them provide the same primary key to identify employees
;and every file begins with a tag identifying the company division it belongs to.
(define (file-tag file) (car file))
(define (get-record employee file)
  (let ((division (file-tag file)))
    (let ((empdata ((get 'get-record division) employee file)))
      (if (null? empdata)
          #f
          empdata))))
;B)
;This requires that each individual employee record begins with a tag identifying the division he works in.
(define (get-salary employee record)
  (let ((division (file-tag record)))
    (let ((empsalary ((get 'get-salary division) employee record)))
      (if (null?? empsalary)
          #f
          empsalary))))
;C)
(define (find-employee-record employee filelist)
  (if (null? filelist)
      #f
      (let ((currentrecord (get-record employee (car filelist))))
        (if currentrecord
            currentrecord
            (find-employee-record employee (cdr filelist))))))
;D)
;All employee files and employee records need to be tagged with the company division they work for. Methods
;to access data need to be installed in the central table.

;exercise 2.75
(define (make-from-mag-ang-2 x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG-2" op))))
  dispatch)

;exercise 2.76
;Generic Operations style:
;For a new type, new procedures need to be written, and intermediary layer procedures need to be changed
;to make use of these new procedures. For a new operation, no change is needed.
;Data-directed style:
;For a new type, new procedures need to be written, put into a package and installed. For a new operation,
;previously defined table and packages need to be modified and reinstalled.
;Message-passing style:
;For a new type, only one large new procedure needs to be written, everything else stays the same.
;For a new operation, no change is needed.
;;;;;;
;For an organization which must add new types often, data-directed style is the best.
;For an organization which must add new operations often, message-passing style is the better choice.

;section 2.5

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;exercise 2.77
;It works because it creates the missing link. "apply-generic" is invoked twice, once for complex, and then for rectangular.

;exercise 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (install-blah-blah)
;exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))
(define (equ-rat x y)
  (and (equ? (numer x) (numer y))
       (equ? (denom x) (denom y))))
(define (equ-comp x y)
  (and (equ? (real-part x) (real-part y))
       (equ? (imag-part x) (imag-part y))))
(put 'equ? '(scheme-number scheme-number) =)
(put 'equ? '(rational rational) equ-rat)
(put 'equ? '(complex complex) equ-comp))

(define (install-blih-blih)
;exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))
(define (zero-scheme x) (lambda (x) (= x 0)))
(define (zero-rat x) (lambda (x) (= (numer x) (denom x) 0)))
(define (zero-comp x) (lambda (x) (= (real-part x) (imag-part x) 0)))
(put '=zero? 'scheme-number zero-scheme)
(put '=zero? 'rational zero-rat)
(put '=zero? 'complex zero-comp))

(define (install-more-sicp-code)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)
  (put-coercion 'scheme-number 'scheme-number
                scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex)
  (define (exp x y) (apply-generic 'exp x y))
  (put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))) ; using primitive expt

;exercise 2.81
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (apply-generic op a1 a2)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags))))))))

;exercise 2.82
(define (and-map op values)
  (cond ((empty? values) #t)
        ((op (car values)) (and-map op (cdr values)))
        (else #f)))
(define (can-coerce? type)
  (lambda (t)
    (if (or (get-coercion type t) (equal? type t))
        #t
        #f)))
(define (find-common-denominator tags)
  (define (inner checked checking tocheck)
    (cond ((and-map (can-coerce? checking) (append checked tocheck)) checking)
          ((empty? tocheck) (error "no coercion possible for these types" tags))
          (else (inner (cons checking checked) (car tocheck) (cdr tocheck)))))
  (inner '() (car tags) (cdr tags)))
(define (multi-coerce coerceto args)
  (define (inner coerceto args result)
    (if (empty? args)
        result
        (inner coercoto
               (cdr args)
               (cons ((get-coercion coerceto (caar args)) cdar args) result))))
  (inner coerceto args '()))
(define (apply-generic-mul op . args)
  (let ((type-tags (map type-tag args)))
    (let ((com-dem (find-common-denominator type-tags)))
      (let ((coerced-args (multi-coerce com-dem args)))
        (apply-generic op coerced-args)))))

;exercise 2.83
(define (install-even-more-code)
  (put 'raise 'scheme-number (lambda (x) (make-rational x 1)))
  (put 'raise 'rational (lambda (x) (make-real (/ (number x) (denom x)))))
  (put 'raise 'real (lambda (x) (make-complex-from-real-imag x 0))))
(define (raise nr)
  (apply-generic 'raise nr))

;exercise 2.84
(define (super-raise x y)
  (define (inner num1 num2)
    (cond ((equ? (type-tag (raise num1)) (type-tag num1)) #f)
          ((equ? (type-tag num1) (type-tag num2)) (list num1 num2))
          (else (inner1 (raise num1) num2))))
  (let ((first-attempt (inner x y))
        (second-attempt (inner y x)))
    (cond (first-attempt first-attempt)
          (second-attempt second-attempt)
          (else (error "No common ground for these types" (list x y))))))
(define (apply-generic-raise op . args)
  (apply op (map contents (super-raise (car args) (cadr args)))))

;exercise 2.85
(define (install-even-more-when-will-I-finally-get-working-put?)
  (put 'project 'complex (lambda (x) (make-real (real-part x))))
  (put 'project 'real (lambda (x) (cond ((= 0 (modulo x 1)) (make-rational x 1))
                                        ((= 0 (modulo (* 10 x) 1)) (make-rational (* 10 x) 10))
                                        ((= 0 (modulo (* 100 x) 1)) (make-rational (* 100 x) 100))
                                        (else (make-rational (- x (/ (modulo (* 100 x) 1) 100)) 100)))))
  (put 'project 'rational (lambda (x) (round (/ (number x) (denom x))))))
(define (drop x)
  (let ((dropped (apply-generic 'project x)))
    (let ((raised (apply-generic 'raise dropped)))
      (if (equ? raised dropped)
          (drop dropped)
          x))))
(define (apply-generic-drop op . args)
  (drop (apply op (map contents (super-raise (car args) (cadr args))))))

;exercise 2.86
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (sq-root x) (apply-generic 'sq-root x))
(define (square x) (apply-generic 'square x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (ratio x) (/ (numer x) (denom x)))
(define (install-jesus-give-me-the-goddamn-put-already)
  (put 'sine 'scheme-number (lambda (x) (make-real (sin x))))
  (put 'cosine 'scheme-number (lambda (x) (make-real (cos x))))
  (put 'sq-root 'scheme-number (lambda (x) (make-real (sqrt x))))
  (put 'square 'scheme-number (lambda (x) (* x x)))
  (put 'arctan '(scheme-number scheme-number) (lambda (x y) (make-real (atan x y))))
  (put 'sine 'rational (lambda (x) (make-real (sin (ratio x)))))
  (put 'cosine 'rational (lambda (x) (make-real (cos (ratio x)))))
  (put 'sq-root 'rational (lambda (x) (make-real (sqrt (ratio x)))))
  (put 'square 'rational (lambda (x) (mul-rat x x)))
  (put 'atan '(rational rational) (lambda (x y) (make-real (atan (ratio x) (ratio y)))))
  (put 'sine 'real (lambda (x) (make-real (sin x))))
  (put 'cosine 'real (lambda (x) (make-real (cos x))))
  (put 'sq-root 'real (lambda (x) (make-real (sqrt x))))
  (put 'square 'real (lambda (x) (make-real (* x x))))
  (put 'atan '(real real) (lambda (x y) (make-real (atan x y)))))
(define (magnitude x)
  (sq-root (add (square (real-part x))
                (square (imag-part x)))))
(define (angle x)
  (arctan (imag-part x) (real-part x)))
(define (make-from-mag-ang m a)
  (cons (mul m (cosine a)) (mul m (sine a))))
(define (real-part x)
  (mul (magnitude x) (cosine (angle x))))
(define (imag-part x)
  (mul (magnitude x) (cosine (angle x))))
(define (make-for-real-imag x y)
  (make-from-mag-ang (sq-root (add (square x) (square y)) (arctan y x))))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;procedures same-variable? and variable? from section 2.3.2

  ;; representation of terms and term lists
  ;procedures adjoin-term ... coeff from text below

  ;; continued on next page

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  ;procedures used by add-poly
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  ;procedures used by mul-poly

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (install-polynomial-package-2)
;exercise 2.87
(put '=zero? 'polynomial (lambda (x) (andmap (lambda (y) (=zero? y)) (map coeff x))))

;exercise 2.88
(define (negate-poly p)
  (make-poly (variable p) (mul-term (term-list p) '((0 -1)))))
(put 'sub 'polynomial (lambda (x y) (add-poly x (negate-poly y))))

;exercise 2.89
(define (first-term-dense term-list) (make-term (- (len term-list) 1) (car term-list)))
(define (adjoin-term-dense term term-list)
  (cond ((=zero? term) term-list)
        ((=equ? (order term) (length term-list)) (cons (coeff term) term-list))
        (else (adjoin-term-dense term (cons 0 term-list)))))
'done)

;exercise 2.90
(define (install-general-polynomial-package)
  (define (the-empty-termlist) '())
  (define (first-term term-list) (apply-generic 'first term-list))
  (define (rest-terms term-list) (cddr term-list))
  (define (empty-termlist? term-list) (null? (cdr term-list)))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (adjoin-term term term-list)
    (apply-generic 'adjoin term term-list))
  (define (tag p) (attach-tag 'polynomial p))
  (define (negate-poly p)
    (make-poly (variable p) (mul-term (term-list p) '((0 -1)))))
  (define (=zero?-poly x)
    (cond ((empty-termlist? x) #t)
          ((=zero? (first-term x)) (=zero?-poly (rest-terms x)))
          (else #f)))
  (put 'sub 'polynomial (lambda (x y) (add-poly x (negate-poly y))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial (lambda (x) (=zero?-poly (term-list x))))
  'done)
(define (install-dense-polynomial-package)
  (define (tag p) (attach-tag 'dense p))
  (put 'first 'dense (lambda (x) (make-term (- (len x) 1) (car x))))
  (define (adjoin-term-dense term term-list)
    (cond ((=zero? term) (tag term-list))
          ((=equ? (order term) (length term-list)) (tag (cons (coeff term) term-list)))
          (else (adjoin-term-dense term (cons 0 term-list)))))
  (put 'adjoin '(polynomial dense) (lambda (x l) (adjoin-term-dense x l)))
  'done)
(define (install-sparse-polynomial-package)
  (define (tag p) (attach-tag 'sparse p))
  (put 'first 'sparse (lambda (x) (car x)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        (tag term-list)
        (tag (cons term term-list))))
  (put 'adjoin '(polynomial sparse) (lambda (x l) (adjoin-term x l)))
  'done)

;exercise 2.91
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms (sub-terms L1
                                           (mul-term-by-all-terms (make-term new-o new-c)
                                                                  L2))
                                L2)))
                (list (adjoin-term (make-term new-o new-c)
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))
(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- DIV-POLY" (list p1 p2))))

;exercise 2.92
"By imposing an ordering on variables, extend the polynomial package so that addition
and multiplication of polynomials works for polynomials in different variables."
;TODO: write hundreds of lines of code for a branch of mathematics that I'm not too familiar with.

;exercise 2.93
