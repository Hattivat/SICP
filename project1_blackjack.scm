;Programming Project 1: Twenty-One

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

;exercise 1
(define (best-total hand)
  (define (rate-card card)
    (cond ((equal? card 'a) 11)
          ((equal? card '10) 10)
          ((equal? card '9) 9)
          ((equal? card '8) 8)
          ((equal? card '7) 7)
          ((equal? card '6) 6)
          ((equal? card '5) 5)
          ((equal? card '4) 4)
          ((equal? card '3) 3)
          ((equal? card '2) 2)
          (else 10)))
  (define (sort-hand hand)
    (cond ((empty? hand) '())
          ((= (length hand) 1) hand)
          ((> (rate-card (bl (first hand))) (rate-card (bl (last hand)))) (se (sort-hand (bl hand)) (last hand)))
          ((= (rate-card (bl (first hand))) (rate-card (bl (last hand)))) hand)
          (else (sort-hand (se (last hand) (sort-hand (se (bl (bf hand)) (first hand))))))))
  (define (bt-inner hand result)
    (cond ((empty? hand) result)
          ((equal? (rate-card (bl (last hand))) 11) (if (> (+ result 11) 21)
                                             (bt-inner (bl hand) (+ result 1))
                                             (bt-inner (bl hand) (+ result 11))))
          (else (bt-inner (bl hand) (+ result (rate-card (bl (last hand))))))))
  (bt-inner (sort-hand hand) 0))

;exercise 2
(define (stop-at-17 hand dealer-card)
  (if (< (best-total hand) 17)
      #t
      #f))

;exercise 3
(define (play-n strategy n)
  (if (= n 0)
      0
      (+ (twenty-one strategy) (play-n strategy (- n 1)))))

;exercise 4
(define (dealer-sensitive hand dealer-card)
  (if (or (and (< (best-total hand) 17)
               (or (equal? (bl dealer-card) 'a)
                   (equal? (bl dealer-card) 'k)
                   (equal? (bl dealer-card) 'j)
                   (equal? (bl dealer-card) 'q)
                   (equal? (bl dealer-card) '7)
                   (equal? (bl dealer-card) '8)
                   (equal? (bl dealer-card) '9)
                   (equal? (bl dealer-card) '10)))
          (and (< (best-total hand) 12)
               (or (equal? (bl dealer-card) '6)
                   (equal? (bl dealer-card) '5)
                   (equal? (bl dealer-card) '4)
                   (equal? (bl dealer-card) '3)
                   (equal? (bl dealer-card) '2))))
      #t
      #f))

;exercise 5
(define (stop-at n)
  (lambda (hand dealer-card)
    (if (< (best-total hand) n)
      #t
      #f)))

;exercise 6
(define (valentine hand dealer-card)
  (define (find-heart hand)
    (cond ((empty? hand) #f)
          ((equal? (last (last hand)) 'h) #t)
          (find-heart (bl hand))))
  (if (or (< (best-total hand) 17) (and (find-heart hand) (< (best-total hand) 19)))
      #t
      #f))

;exercise 7
(define (suit-strategy suit strat-yes strat-no)
  (define (find-suit hand suit)
    (cond ((empty? hand) #f)
          ((equal? (last (last hand)) suit) #t)
          (find-suit (bl hand))))
  (lambda (hand dealer-card)
    (if (find-suit hand suit)
        (strat-yes hand dealer-card)
        (strat-no hand dealer-card))))

;exercise 8
(define (majority strat1 strat2 strat3)
  (lambda (hand dealer-card)
    (if (or (and (strat1 hand dealer-card) (strat2 hand dealer-card))
            (and (strat2 hand dealer-card) (strat3 hand dealer-card))
            (and (strat1 hand dealer-card) (strat3 hand dealer-card)))
        #t
        #f)))

;exercise 9
(define (reckless strat)
  (lambda (hand dealer-card)
    (strat (bl hand) dealer-card)))