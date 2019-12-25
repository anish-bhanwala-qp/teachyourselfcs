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

# Exercise 2.4
```
(define (cons1 x y)
  (lambda (m) (m x y)))
(define (car1 z)
  (z (lambda (p q) p)))
(define (cdr1 z)
  (z (lambda (p q) q)))
```

# Exercise 2.5
```
(define (exp x y)
  (define (square a) (* a a))
  (define (exp-iter i result)    
    (cond ((= i 1) result)
          ((= (remainder i 2) 0)
           (exp-iter (/ i 2) (square result)))
          (else (* result (exp-iter (- i 1) result)))))
  (exp-iter y x))

(define (cons2 x y)
  (* (exp 2 x) (exp 3 y)))

(define (find-exp x val)
  (define (find-exp-iter i prev)
    (if (= (remainder x prev) 0)
        (find-exp-iter (+ i 1) (* prev val))
        i))
  (find-exp-iter 0 val))

(define (car2 x)  
  (find-exp x 2))
(define (cdr2 x)  
  (find-exp x 3))

(define p2 (cons2 3 12))
(car2 p2)
(cdr2 p2)
```

# Exercise 2.7
```
(define (lower-bound interval)
  (car interval))
(define (upper-bound interval)
  (cdr interval))
```

# Exercise 2.8
```
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
```

# Exercise 2.17
```
(define (last-pair l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))
```

# Exercise 2.18
```
(define (reverse li)
  (define (reverse-internal remaining prev)    
    (if (null? (cdr remaining))
        (cons (car remaining) prev)
        (reverse-internal (cdr remaining) (cons (car remaining) prev))))
  (reverse-internal li nil))
```

# Exercise 2.19
```
(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))
```

# Exercise 2.20
Iterative can be done by using **append**. Also its easier to construct reverse order list. 
```
(define (same-parity . w)
  (define (odd? x) (= (remainder x 2) 1))
  (define (even? x) (= (remainder x 2) 0))
  (define (same-parity-recursive items parity-check)
    (cond ((null? items) items)
          ((parity-check (car items))
           (cons (car items) (same-parity-recursive (cdr items) parity-check)))
          (else (same-parity-recursive (cdr items) parity-check))))
  (cond ((null? w) w)
        ((odd? (car w)) (cons (car w) (same-parity-recursive (cdr w) odd?)))
        (else (cons (car w) (same-parity-recursive (cdr w) even?)))))
```
