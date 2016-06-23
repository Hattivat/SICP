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
  (if (and (not (null? y)) (list? y))
      (cons (same-parity x . (car y)) (same-parity x . (cdr y)))
      (if (= (modulo x 2) (modulo y 2))
          y
          '())))