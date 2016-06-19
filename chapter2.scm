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

;exercise 2.6
