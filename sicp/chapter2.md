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

# Exercise 2.21
```
(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list1 items)
  (map square items))
```

# Exercise 2.23
```
(define (for-each fn items)
  (cond ((null? items) true)
      (else (fn (car items))
      (for-each fn (cdr items)))))

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4 5))
```

# Exercise 2.24
The box structure is: (1, )-> ((2, )-> ((3, )-> (4, nil), nil), nil)
```
(list 1 (list 2 (list 3 4)))
;;(mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))
```

# Exercise 2.25
```
;;(1 3 (5 7) 9)
(define l (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l)))))

;;((7))
(define l2 (list (list 7)))
(car (car l2))

;;(1 (2 (3 (4 (5 (6 7))))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))
```

# Exercise 2.26
```
(append x y)
(cons x y)
(list x y)
;;(mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 (mcons 6 '()))))))
;;(mcons (mcons 1 (mcons 2 (mcons 3 '()))) (mcons 4 (mcons 5 (mcons 6 '()))))
;;(mcons (mcons 1 (mcons 2 (mcons 3 '()))) (mcons (mcons 4 (mcons 5 (mcons 6 '()))) '()))
```

# Exercise 2.27
```
(define (deep-reverse items)
  (define (reverse-car-if-pair x)
    (if (pair? x)
        (deep-reverse-iter x nil)
        x))
    (define (deep-reverse-iter remaining earlier-items)
      (cond ((null? remaining) earlier-items)          
            (else (deep-reverse-iter (cdr remaining) (cons (reverse-car-if-pair (car remaining)) earlier-items)))))
    (deep-reverse-iter items nil))
```

# Exercise 2.28
```
(define (fringe items)
  (if (null? items)
      nil
      (let ((x (car items))
            (y (cdr items)))
        (cond ((null? x) nil)
              ((pair? x) (append (fringe x) (fringe y)))
              (else (cons x (fringe y)))))))
```

# Exercise 2.29
For the **2.29(d)** question, which need to change two procedures i.e. `righ-branch` and `branch-structure`.
```
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (cond ((pair? structure) (total-weight structure))
        (else structure))))

(define (total-weight mobile)  
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced mobile)
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))
```

# Exercise 2.30
```
(define (square-tree t)
  (cond ((null? t) t)
        ((not (pair? t)) (* t t))
        (else (cons (square-tree (car t)) (square-tree (cdr t))))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree-map t)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-map x)
             (* x x)))
       t))

(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
```

# Exercise 2.31
```
(define (tree-map f t)
  (cond ((null? t) t)
        ((not (pair? t)) (f t))
        (else (cons (tree-map f (car t)) (tree-map f (cdr t))))))

(define (square-tree-map2 t)
  (tree-map (lambda (x) (* x x)) t))

(square-tree-map2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
```

# Exercise 2.33
```
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3 4 5 6))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (* x x)) (list 1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))
```
