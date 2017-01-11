;sample exams for midterm 1

;sample exam 1

;problem 1
(every - (keep number? '(the 1 after 909)))
;scheme will print '(-1 -909) because it applies - to every item left after selecting only numbers from the provided list.
((lambda (a b) ((if (< b a) + *) b a)) 4 6)
;scheme will print 24 because 6 is not smaller than 4, so the function selects *.
(word (first '(cat)) (butlast 'dog))
;scheme will print catdo, because the function "first" is passed a list with one element, so it selects this one element.
(cons (list 1 2) (cons 3 4))
; scheme will print '((1 2) 3 . 4) and the box-and-pointer diagram would be:
;[|][=]>[|][|]
; v      V  V
;[|][|]  3  4
; V  v
; 1 [|][/]
;    v
;    2
(let ((p (list 4 5)))
  (cons (cdr p) (cddr p)))
;scheme will print '((5)) and the box-and-pointer diagram would be:
;[|][/]
; v
;[|][/]
; v
; 5
(cadadr '((a (b) c) (d (e) f) (g (h) i))
;error, lacks closing brace.
)
;after adding it, it will print '(e), box-and-pointer:
;[|][/]
; v
; e

;problem 2
(define (foo n)
  (if (< n 2)
      1
      (+ (baz (- n 1))
         (baz (- n 2)))))
(define (baz n)
  (+ n (- n 1)))
;this is an O(1) function.
(define (garply n)
  (if (= n 0)
      0
      (+ (factorial n) (garply (- n 1)))))
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
;this is an O(n^2) function.

;problem 3
;under applicative order the answer is 2.
;under normal order the answer is also 2.

;problem 4
;the following procedures generate an iterative process in spite of their appearance:
(define (butfirst-n num stuff)
  (if (= num 0)
      stuff
      (butfirst-n (- num 1) (bf stuff))))
(define (member? thing stuff)
  (cond ((empty? stuff) #f)
        ((equal? thing (first stuff)) #t)
        (else (member? thing (bf stuff)))))

;problem 5
(define (vowel? letter)
  (member? letter '(a e i o u)))
(define (syllables aword)
  (cond ((empty? aword) 0)
        ((empty? (bf aword)) (if (vowel? aword)
                                 1
                                 0))
        ((and (vowel? (first aword)) (not (vowel? (first (bf aword))))) (+ 1 (syllables (bf aword))))
        (else (syllables (bf aword)))))

;problem 6
(define (in-order? func alist)
  (cond ((empty? (cdr alist)) #t)
        ((func (car alist) (cadr alist)) (in-order? func (cdr alist)))
        (else #f)))
(define (order-checker func)
  (lambda (sentence) (in-order? func sentence)))

;problem 7
(define (make-time hr mn cat) (list hr mn cat))
(define hour car)
(define minute cadr)
(define category caddr)
(define (time-print-form time)
  (print (hour time))(print ':)(print (minute time))(print (category time)))
(define (24-hour time)
  (if (equal? (category time) 'pm)
      (word (+ 12 (hour time)) (minute time))
      (word (hour time) (minute time))))
(define (make-time hr mn cat)
  (if (equal? cat 'pm)
      (list (+ 12 hr) mn)
      (list hr mn)))
(define (hour time)
  (let ((h (car time)))
    (if (> h 11)
        (- car 12)
        h)))
(define minute cadr)
(define (category time)
  (if (> (car time) 11)
      'pm
      'am))

;sample exam 2

;problem 1
;(every (bf x) '(ab cd ef gh))
;error, because x is not defined.
(cond ('hello 5) (#t 6) (else 7))
;Scheme prints 5, because all values count as true, and 'hello is the first one to be evaluated.
;(let ((x 10)
;      (y (+ x 2)))
;  (* y 3))
;Error because this would need nested "let"s, otherwise x in not bound.
(cons (list '() '(b)) (append '(c) '(d)))
;Scheme will print ((() (b)) c d). The box-and-pointer diagram would be:
;[|][=]>[|][=]>[|][/]
; |      v      v
; V      c      d
;[/][=]>[|][/]
;        v
;       [|][/]
;        v
;        b
((lambda (x) (cons x x)) '(a))
;Scheme will print '((a) a), and the box-and-pointer diagram would be:
;[|][=]>a
; V
;[|][/]
; v
; a
(cdar '((1 2) (3 4)))
;Scheme will print '(2), box-and-pointer obviously:
;[|][/]
; v
; 2

;problem 2
(define (garply n)
  (if (< n 20)
      n
      (+ (foo n)
         (garply (- n 1)))))
;False. We can determine that the order of growth of garply is at least O(n), but it could be higher, depending on foo.
;True, it is at a minimum O(n).
;True, in this case it would be O(n^2).
;Flase, garply does not generate an iterative process.

;problem 3
(define (mountain x) 'done)
(define (dew) (dew))
;the expression (mountain (dew)) in 'done in normal order and an infinite loop in applicative order.
;the expression (mountain dew) will result in 'done in both normal and applicative orders.

;problem 4
(define (every-nth n sntc)
  (define (inner x n sntc)
    (cond ((empty? sntc) '())
          ((= x 1) (se (first sntc) (inner n n (bf sntc))))
          (else (inner (- x 1) n (bf sntc)))))
  (inner n n sntc))

;problem 5
(define (differences sent)
  (if (empty? (bf sent))
      '()
      (se (- (first sent) (first (bf sent)))
          (differences (bf sent)))))
(define (wordpairs sent)
  (if (empty? (bf sent))
      '()
      (se (word (first sent) (first (bf sent)))
          (wordpairs (bf sent)))))
(define (pairmap sent op)
  (if (empty? (bf sent))
      '()
      (se (op (first sent) (first (bf sent)))
          (pairmap (bf sent) op))))
(define (differences2 sent)
  (pairmap sent -))
(define (wordpairs2 sent)
  (pairmap sent word))

;problem 6
(define (colors sockdrawer)
  (define (remdup seq)
    (cond ((null? seq) '())
          ((memq (car seq) (cdr seq)) (remdup (cdr seq)))
          (else (cons (car seq) (remdup (cdr seq))))))
  (remdup sockdrawer))
(define (howmany color sockdrawer)
  (length (filter (lambda (sock) (eq? sock color)) sockdrawer)))
(define (odd-sock? sockdrawer)
  (define (inner)
    (cond ((null? colors) #f)
          ((odd? (howmany (car colors) sockdrawer)) #t)
          (else (inner (cdr colors)))))
  (inner (colors sockdrawer)))
(define (colors-new sockdrawer)
  (if (null? sockdrawer)
      '()
      (cons (caar sockdrawer) (colors-new (cdr sockdrawer)))))
(define (howmany-new color sockdrawer)
  (cond ((null? sockdrawer) 0)
        ((equal? (caar sockdrawer) color) (cadar sockdrawer))
        (else (howmany-new color (cdr sockdrawer)))))