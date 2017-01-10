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
;[|][=]>[3][4]
; v
;[1][|]
;    v
;   [2]['()]
(let ((p (list 4 5)))
  (cons (cdr p) (cddr p)))
;scheme will print '((5)) and the box-and-pointer diagram would be:
;[|]['()]
; v
;[5]['()]
(cadadr '((a (b) c) (d (e) f) (g (h) i))
;error, lacks closing brace.
)
;after adding it, it will print '(e).

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