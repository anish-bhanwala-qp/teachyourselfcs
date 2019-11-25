# Exercise 1.2
```
(/ (+ 5 5 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
```
   
# Exercise 1.3
```
(define (sum-of-square a b) (+ (* a a) (* b b) ) )

(define (two-greater-num-square a b c)
   (if (> a b) 
      (if (> b c) 
         (sum-of-square a b) 
         (sum-of-square a c)) 
      (if (> a c) 
         (sum-of-square a b) 
         (sum-of-square b c)) )) 
```

# Exercise 1.4
```
(define (a-plus-abs-b a b)
   ((if (> b 0) + -) a b))
```
This procedure adds value of **a** to absolute value of **b** by returning an *operator* from if condition instead of the ususal values.

# Exercise 1.5
```
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))
```
For applicative-order the expression **p** is evaluted recursively because it returns itself. While for normal-order the  procedure **(p)** body is substituted while evaluating and then evaluated. So it doesn't result in infinite loop. 

# Exercise 1.6
sqrt-iter should not be called if the guess is *good-enough*. But in the case of *new-if* both the arguments are evaluated (applicative-order) causing infinite loop. 

# Exercise 1.9
```
(define (+ a b)
(if (= a 0) b (inc (+ (dec a) b))))
```
Above procedure is recursive. As the increment operation **(inc (+ dec a) b)** depends upon the result of the recursive call.
```
(define (+ a b)
(if (= a 0) b (+ (dec a) (inc b))))
```
Whereas in the above procedure the recursive call has all the information at any point to represent current state of the program. Also the there is no operation waiting to be executed based on the result of the recursive call other than returning the result which can be returned whenever the final result is computed.

# Exercise 1.10
 - (A 0 10) output is 2^10
 - (A 2 4) outpout is 2^2^4
 - (A 3 3) outpout is 2^2^2^3

Mathematical representations:
- `(define (f n) (A 0 n))` **2*n**
- `(define (g n) (A 1 n))` **2^n**
- `(define (h n) (A 2 n))` **2^2^n**

# Exercise 1.11
Recursive
```
(define (f-rec n)
  (cond ((< n 3) n)
        (else (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3)))))))
```
Iterative
```
(define (f n)
  (cond ((< n 3) n)
      (else (f-iter 0 1 2 3 n))))

(define (f-iter p1 p2 p3 count n)
  (define p4 (+ p3 (* 2 p2) (* 3 p1 )))
  (cond ((= count n) p4)        
        (else (f-iter p2 p3 p4 (+ count 1) n))))
```
