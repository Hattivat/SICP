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

;week 5
(define (mystery L1 L2)
  (append (cons L2 L1) L2))
;the rest was exercises from chapter2, solved in the other file.

;week 6
(lambda (alist n)
  ((lambda (find-nth) (find-nth find-nth alist n))
   (lambda (find-nth alist n)
     (if (null? alist)
         '()
         (if (= n 0)
             (car alist)
             (find-nth find-nth (cdr alist) (- n 1)))))))

(define (eval-1 exp)
  (cond ((constant? exp) exp)
	((symbol? exp) (eval exp))	; use underlying Scheme's EVAL
	((quote-exp? exp) (cadr exp))
	((if-exp? exp)
	 (if (eval-1 (cadr exp))
	     (eval-1 (caddr exp))
	     (eval-1 (cadddr exp))))
        ((and-exp? exp)
         (if (eval-1 (cadr exp))
             (eval-1 (caddr exp))
             #f))
        ((map-exp? exp)
         
	((lambda-exp? exp) exp)
	((pair? exp) (apply-1 (eval-1 (car exp))      ; eval the operator
			      (map eval-1 (cdr exp))))
	(else (error "bad expr: " exp))))

(define and-exp? (exp-checker 'and))

(define map-exp? (exp-checker 'map))

;scheme-1 code from the course:
(define (scheme-1)
  (display "Scheme-1: ")
  (flush-output)
  (print (eval-1 (read)))
  (newline)
  (scheme-1))
  
(define (apply-1 proc args)
  (cond ((procedure? proc)	; use underlying Scheme's APPLY
	 (apply proc args))
	((lambda-exp? proc)
	 (eval-1 (substitute (caddr proc)   ; the body
			     (cadr proc)    ; the formal parameters
			     args           ; the actual arguments
			     '())))	    ; bound-vars, see below
	(else (error "bad proc: " proc))))
  
(define (constant? exp)
  (or (number? exp) (boolean? exp) (string? exp) (procedure? exp)))

(define (exp-checker type)
  (lambda (exp) (and (pair? exp) (eq? (car exp) type))))

(define quote-exp? (exp-checker 'quote))
(define if-exp? (exp-checker 'if))
(define lambda-exp? (exp-checker 'lambda))
  
(define (substitute exp params args bound)
  (cond ((constant? exp) exp)
	((symbol? exp)
	 (if (memq exp bound)
	     exp
	     (lookup exp params args)))
	((quote-exp? exp) exp)
	((lambda-exp? exp)
	 (list 'lambda
	       (cadr exp)
	       (substitute (caddr exp) params args (append bound (cadr exp)))))
	(else (map (lambda (subexp) (substitute subexp params args bound))
		   exp))))

(define (lookup name params args)
  (cond ((null? params) name)
	((eq? name (car params)) (maybe-quote (car args)))
	(else (lookup name (cdr params) (cdr args)))))

(define (maybe-quote value)
  (cond ((lambda-exp? value) value)
	((constant? value) value)
	((procedure? value) value)	; real Scheme primitive procedure
	(else (list 'quote value))))