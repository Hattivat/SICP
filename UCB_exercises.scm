;week 1
(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

(define (ordered? sent)
  (cond ((empty? (bf sent)) #t)
        ((< (first (bf sent)) (first sent)) #f)
        (else (ordered? (bf sent)))))

(define (ends-e sent)
  (cond ((empty? sent) '())
        ((equal? (last (first sent)) 'e) (sentence (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent)))))

(define (squares sent)
  (cond ((empty? sent) '())
        (else (sentence (* (first sent) (first sent)) (squares (bf sent))))))

(define (switch sent)
  (if (equal? (first sent) 'You)
      (sentence 'I (switch-two (bf sent)))
      (switch-two sent)))

(define (switch-two sent)
  (cond ((empty? sent) '())
        ((or (equal? (first sent) 'I) (equal? (first sent) 'me))
         (sentence 'you (switch-two (bf sent))))
        ((equal? (first sent) 'you)
         (sentence 'me (switch-two (bf sent))))
        (else (sentence (first sent) (switch-two (bf sent))))))

(define (dupls-removed sen)
  (cond ((empty? sen) '())
      ((member? (first sen) (bf sen))
          (dupls-removed (bf sen)))
          (else (sentence (first sen) (dupls-removed (bf sen))))))

;week 2
(define (every f sent)
  (if (empty? sent)
      '()
      (sentence (f (first sent)) (every f (bf sent)))))

(define (substitute sent oldw neww)
  (cond ((empty? sent) '())
        ((equal? (first sent) oldw) (sentence neww (substitute (bf sent) oldw neww)))
        (else (sentence (first sent) (substitute (bf sent) oldw neww)))))

(define (t f)
  (lambda (x) (f (f (f x)))) )
(define (1+ x)
  (+ x 1))

(define (make-tester w)
  (lambda (x) (equal? w x)))

;week 3
(define (next-perf n)
  (define (sum-of-factors res n i)
    (if (= i n)
        res
        (if (= 0 (modulo n i))
            (sum-of-factors (+ res i) n (+ i 1))
            (sum-of-factors res n (+ i 1)))))
  (if (= n (sum-of-factors 0 n 1))
      n
      (next-perf (+ n 1))))

(define (number-of-partitions n)
  (define (nop n x)
    (cond ((= n 0) 1)
          ((or(< n 0) (= x 0)) 0)
          (else (+ (nop n (- x 1)) (nop (- n x) x)))))
  (nop n n))

(define (count-change amount)
  (cc amount '(50 25 10 5 1)))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (empty? kinds-of-coins)) 0)
        (else (+ (cc amount
                     (bf kinds-of-coins))
                 (cc (- amount
                        (first kinds-of-coins))
                     kinds-of-coins)))))

(define (type-check func test x)
  (if (test x)
      (func x)
      #f))

(define (make-safe func test)
  (lambda (x) (if (test x)
                  (func x)
                  #f)))

;week 4
(define (+rat a b)
    (make-rational (+ (numerator a) (numerator b)) (+ (denominator a) (denominator b))))

(define (substitute l old new)
  (if (eqv? l old)
      new
      (if (and (list? l) (not (null? l)))
          (cons (substitute (car l) old new) (substitute (cdr l) old new))
          l)))

(define (substitute2 l old new)
  (define (test-word x o n)
    (if (null? o)
        x
        (if (eqv? (car o) x)
            (car n)
            (test-word x (cdr o) (cdr n)))))
  (if (and (list? l) (not (null? l)))
      (cons (substitute2 (car l) old new) (substitute2 (cdr l) old new))
      (test-word l old new)))

(define (cxr-function w)
  (define (inner w x)
    (cond ((eqv? (first w) 'c) (inner (bf w) x))
          ((eqv? (first w) 'a) (car (inner (bf w) x)))
          ((eqv? (first w) 'd) (cdr (inner (bf w) x)))
          ((eqv? (first w) 'r) x)
          (else (display 'Error!))))
  (lambda (x) (inner w x)))