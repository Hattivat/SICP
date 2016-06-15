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