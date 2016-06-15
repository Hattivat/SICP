;exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;exercise 1.3
(define (myproc x y z)
  (cond ((and (> x y) (> x z))
         (cond ((> y z) (+ (* x x) (* y y)))
               (else (+ (* x x) (* z z)))))
         ((and (> y x) (> y z))
          (cond ((> x z) (+ (* y y) (* x x)))
                (else (+ (* y y) (* z z)))))
         (else (cond ((> x y) (+ (* z z) (* x x)))
                     (else (+ (* z z) (* y y)))))))
(define (myupgproc a b c)
  (cond ((and (< a b) (< a c)) (+ (* b b) (* c c)))
        ((and (< b a) (< b c)) (+ (* a a) (* c c)))
        (else (+ (* a a) (* b b)))))

(define (sqrt x)
  (my-sqrt 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;exercise 1.7
(define (my-sqrt guess x)
  (define (my-good? change x)
    (< (abs (/ change x)) 0.001))
  (if (my-good? (abs (- (improve guess x) guess)) x)
      guess
      (my-sqrt (improve guess x) x)))

;exercise 1.8
(define (cube-root x)
  (define (cube-improve guess x)
    (/ (+ (/ x (* guess guess)) (* guess 2)) 3))
  (define (my-good? change x)
    (< (abs (/ change x)) 0.0001))
  (define (cbrt-iter guess x)
    (if (my-good? (abs (- (cube-improve guess x) guess)) x)
        guess
        (cbrt-iter (cube-improve guess x) x)))
  (cbrt-iter 1.0 x))

;exercise 1.11
(define (weird n)
  ( if (< n 3)
       n
       (+ (weird (- n 1)) (* 2 (weird (- n 2))) (* 3 (weird (- n 3))))))
(define (weird2 n)
  (define (weird-iter n a b c)
    (if (= n 3)
        (+ a (* 2 b) (* 3 c))
        (weird-iter (- n 1) (+ a (* 2 b) (* 3 c)) a b)))
  (if (< n 3)
      n
      (weird-iter n 2 1 0)))

;exercise 1.12
(define (pascal row col)
  (cond ((and (= row 0) (= col 0)) 1)
        ((or (> col row) (< col 0)) 0)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;exercise 1.16
(define (fexpt-iter b n)
  (define (expt-intern a b n)
    (cond ((= n 0) a)
          ((even? n) (expt-intern a (square b) (/ n 2)))
          (else (expt-intern (* a b) b (- n 1)))))
  (expt-intern 1 b n))

;exercise 1.17-1.18
(define (mul a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (mul-iter a b result)
    (cond ((= b 0) result)
          ((even? b) (mul-iter (double a) (halve b) result))
          (else (mul-iter a (- b 1) (+ result a)))))
  (mul-iter a b 0))

;exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a b
                                 (+ (* p p) (* q q))
                                 (+ (* p q) (* q q) (* q p))
                                 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; exercise 1.23
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;exercise 1.27
(define (carmichael n)
  (define (test a)
    (= (expmod a n n) a))
  (define (carm-iter x)
    (cond ((= x 1) #t)
          ((not (test x)) #f)
        (else (carm-iter (- x 1)))))
  (carm-iter (- n 1)))

;exercise 1.28
(define (miller-rabin x)
  (= (expmod (+ 1 (random (- x 1))) (- x 1) x) 1))

(define (mr-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (if (or (= base 1) (= base (- m 1)) (= exp (- m 1)) (not (= (remainder base m) 1)))
                         (remainder (square (expmod base (/ exp 2) m)) m)
                         0))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;exercise 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (sim-sub k)
    (f (+ a (* h k))))
  (define (sim-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             ((even? k) 2))
       (sim-sub k)))
  (* (/ h 3.0) (sum-iter sim-term 0 inc n)))

;exercise 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;exercise 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;exercise 1.33
(define (filtered-accumulate filter? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter? a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a (lambda (x) (+ x 1)) b))

(define (posintlt n)
  (filtered-accumulate (lambda (x n) (= 1 (gcd x n))) * 1 (lambda (x) x) (lambda (x) (+ x 1)) b))

;;;;

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;exercise 1.35
(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;exercise 1.36
(define (fixed-point-p f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define xx (fixed-point-p (lambda (x) (/ (log 1000) (log x))) 2))

;exercise 1.37
(define (cont-franc n d k)
  (define (cf-recur n d i k)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (cf-recur n d (+ i 1) k)))))
  (cf-recur n d 1 k))

;exercise 1.38
(define (euler x) (+ 2 (cont-franc (lambda (x) 1) (lambda (x) (if (= (remainder (+ x 1) 3) 0) (* 2 (/ (+ i 1) 3)) 1)) 10)))

;exercise 1.39
(define (tan-cf x k)
  (define sqx (* x x))
  (define (tan-cf-in x i k)
    (if (> (/ (+ i 1) 2) k)
        0
        (- i (/ sqx (tan-cf-in x (+ i 2) k)))))
  (/ x (tan-cf-in x 1 k)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;exercise 1.40
(define (cubic a b c)
  (+ (* x x x) (* a x x) (* b x) c))

;exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define answer 21)

;exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;exercise 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;exercise 1.44
(define dx 0.1)
(define (aver a b c) (/ (+ a b c) 3))
(define (smooth f)
  (lambda (x) (aver (f x) (f (+ x dx)) (f (- x dx)))))
(define (rep-smooth f n)
  ((repeated smooth n) f))

;exercise 1.45
(define (root n x)
  (fixed-point ((repeated average-damp (- n 2)) (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0))

;exercise 1.46
(define (iter-improve good-enough? improve)
  (lambda (x)
    (if (good-enough? x)
        x
        ((iter-improve good-enough? improve) (improve x)))))
(define (ii-sqrt x)
  (define guess 1.0)
  ((iter-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                (lambda (guess) (/ (+ guess (/ x guess)) 2))) x))
(define (ii-fixed-point f first-guess)
  (define tolerance 0.00001)
  ((iter-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance)) f) first-guess))