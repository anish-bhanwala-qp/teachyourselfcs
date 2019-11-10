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


