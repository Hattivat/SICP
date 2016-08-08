;Section 2.2.4 of SICP
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (for-each proc lst)
  (if (null? lst)
      (newline)
      (and (proc (car lst)) (for-each proc (cdr lst)))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;exercise 2.45
(define (split op1 op2)
  (define (split-inner painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-inner painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  split-inner)

;exercise 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect vect scal)
  (make-vect (* scal (xcor-vect vect))
             (* scal (ycor-vect vect))))

;exercise 2.47
(define (edge-one frame)
  (car (cdr frame)))
(define (edge-two frame)
  (car (cdr (cdr frame))))
(define (frame-origin frame)
  (car frame))
(define (edge-one2 frame)
  (car (cdr frame)))
(define (edge-two2 frame)
  (cdr (cdr frame)))
(define (frame-origin2 frame)
  (car frame))

;exercise 2.48
(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment v)
  (car v))
(define (end-segment v)
  (cdr v))

;exercise 2.49
(define (outline-painter frame)
   ((segments->painter (list (make-segment (make-vector 0 0) (make-vector 0 1))
                             (make-segment (make-vector 0 1) (make-vector 1 1))
                             (make-segment (make-vector 1 1) (make-vector 1 0))
                             (make-segment (make-vector 1 0) (make-vector 0 0)))) frame))
(define (x-painter frame)
  ((segments->painter (list (make-segment (make-vector 0 0) (make-vector 1 1))
                            (make-segment (make-vector 0 1) (make-vector 1 0)))) frame))
(define (diamond-painter frame)
  ((segments->painter (list (make-segment (make-vector 0 0.5) (make-vector 0.5 1))
                            (make-segment (make-vector 0.5 1) (make-vector 1 0.5))
                            (make-segment (make-vector 1 0.5) (make-vector 0.5 0))
                            (make-segment (make-vector 0.5 0) (make-vector 0 0.5)))) frame))
(define (wave-painter frame)
  ((segments->painter (list (make-segment (make-vector 0.25 0) (make-vector 0.35 0.4))
                            (make-segment (make-vector 0.4 0.4) (make-vector 0 0.15))
                            (make-segment (make-vector 0 0.3) (make-vector 0.2 0.65))
                            (make-segment (make-vector 0.2 0.65) (make-vector 0.33 0.65))
                            (make-segment (make-vector 0.33 0.65) (make-vector 0.28 0.8))
                            (make-segment (make-vector 0.28 0.8) (make-vector 0.33 1))
                            (make-segment (make-vector 0.4 0) (make-vector 0.5 0.3))
                            (make-segment (make-vector 0.5 0.3) (make-vector 0.6 0))
                            (make-segment (make-vector 0.75 0) (make-vector 0.65 0.5))
                            (make-segment (make-vector 0.65 0.5) (make-vector 0.75 0.6))
                            (make-segment (make-vector 0.75 0.6) (make-vector 0.85 0.4))
                            (make-segment (make-vector 0.85 0.4) (make-vector 1 0.6))
                            (make-segment (make-vector 1 0.8) (make-vector 0.85 0.6))
                            (make-segment (make-vector 0.85 0.6) (make-vector 0.75 0.65))
                            (make-segment (make-vector 0.75 0.65) (make-vector 0.65 0.65))
                            (make-segment (make-vector 0.65 0.65) (make-vector 0.7 0.8))
                            (make-segment (make-vector 0.7 0.8) (make-vector 0.65 1)))) frame))