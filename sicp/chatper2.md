# Exercise 2.1
```
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (< n 0) (< d 0)) (cons (/ (* -1 n) g) (/ (* d -1) g)))
          ((< d 0) (cons (/ (* -1 n) g) (/ (* d -1) g)))
          (else (cons (/ n g) (/ d g))))))     
```

# Exercise 2.2
```
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (mid-point s)
  (define (average a b) (/ (+ a b) 2))
  (let ((start-s (start-segment s))
        (end-s (end-segment s)))
    (let ((avg-x (average (x-point start-s) (x-point end-s)))
          (avg-y (average (y-point start-s) (y-point end-s))))
      (make-point avg-x avg-y))))
     

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")  
  (display (y-point p))
  (display ")")
  (newline))

(define p1 (make-point 2 3))
(define p2 (make-point 4 9))
(x-point p1)
(y-point p1)

(define s1 (make-segment p1 p2))
(print-point (start-segment s1))
(print-point (end-segment s1))
(print-point (mid-point s1))
```

# Exercise 2.3
```
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-rect top-left-p bottom-right-p)
  (cons top-left-p bottom-right-p))
(define (first-point r)
  (car r))
(define (second-point r)
  (cdr r))

(define (rect-length r)
  (abs (- (x-point (first-point r)) (x-point (second-point r)))))
(define (rect-width r)
  (abs (- (y-point (first-point r)) (y-point (second-point r)))))

(define (alt-make-rect bottom-left-p top-right-p)
  (cons bottom-left-p top-right-p))
(define (alt-rect-length r)
  (abs (- (x-point (first-point r)) (x-point (second-point r)))))
(define (alt-rect-width r)
  (abs (- (y-point (first-point r)) (y-point (second-point r)))))



(define (rect-area r)
  (* (rect-length r) (rect-width r)))
(define (rect-perimeter r)
  (+ (* 2 (rect-length r)) (* 2 (rect-width r))))

(define r1 (make-rect (make-point 0 3) (make-point 3 0)))
(rect-length r1)
(rect-width r1)
(rect-area r1)
(rect-perimeter r1)

(define r2 (alt-make-rect (make-point 0 0) (make-point 3 3)))
(rect-length r2)
(rect-width r2)
(rect-area r2)
(rect-perimeter r2)
```
