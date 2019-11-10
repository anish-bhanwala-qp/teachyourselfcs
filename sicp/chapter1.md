# Exercise 1.2
```
(/ (+ 5 5 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
```
   
# Exercise 1. 3
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
