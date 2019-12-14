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

# Exercise 1.12
```
(define (pascal r c)
  (cond ((or (= c 1) (= r c)) 1)
        (else (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))))
```

# Exercise 1.13
```
;;;;;;;;;; START  square root calculation ;;;;;;;;;;;;
(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
;;;;;;;;;; END ;;;;;;;;;;;;


;;;;;;;;;; START  calculate x raise to power y ;;;;;;;;;;;;
(define (power num exponent)
  (power-iter 1 num exponent num))

(define (power-iter count num exponent result)
  (if (= count exponent)
      result
      (power-iter (+ count 1) num exponent (* result num))))
;;;;;;;;;; END ;;;;;;;;;;;;


;;;;;;;;;; START Fib calculator ;;;;;;;;;;;;
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fib-iter 0 1 (- n 1)))))

(define (fib-iter prev curr count)
  (if (= count 0)
      curr
      (fib-iter curr (+ prev curr) (- count 1))))

;;;;;;;;;; END ;;;;;;;;;;;;

(define (phi)
  (/ (+ 1 (sqrt 5)) 2))

(define (psi)
  (/ (- 1 (sqrt 5)) 2))


;;;;;;;;;; START round decimal not nearest smaller integer ;;;;;;;;;;;;
(define (floor num)
  (if (or (< num 0) (= num 0))
      n
      (sm-iter 1 num)))
(define (sm-iter cnt num)
  (cond ((= cnt num) cnt)
        ((> (+ cnt 1) num) cnt)
        (else (sm-iter (+ cnt 1) num))))
;;;;;;;;;; END ;;;;;;;;;;;;


;;;;;;;;;; START Round off to nearest integer ;;;;;;;;;;;;
(define (roundoff n)
  (nearest-internal (floor n) n))

(define (nearest-internal smaller n)
  (if (or (< (- n smaller) 0.5) (= (- n smaller) 0.5))
      smaller
      (+ smaller 1)))
;;;;;;;;;; END ;;;;;;;;;;;;



;;;;; Verify Fib(n) = (φ^n - ψ^n)/√5. Where ψ(psi) = (1 -√5)/2, φ(phi) = (1 + √5)/2. ;;;;;;;;;
(define (verify n)
  (= (fib n) (roundoff (/ (power (phi) n) (sqrt 5)))))
(verify 7)
```

# Exercise 1.16
```
(define (sq n) (* n n ))

(define (fast-expt2 b n)
  (fast-iter 1 b n))
(define (fast-iter a b n)
  (cond ((= n 0) 1)
        ((= n 1) (* a b))
        ((even? n) (fast-iter a (sq b) (/ n 2)))
        (else (fast-iter (* a b) b (- n 1)))))
```

# Exercise 1.17
```
(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

(fast-mul 3 10)
```

# Exercise 1.18
```
(define (fast-mul-iter a b)
  (mul-iter a b 0))

(define (mul-iter a b add)
  (cond ((= b 0) 0)
        ((= b 1) (+ a add))
        ((even? b) (mul-iter (double a) (halve b) add))
        (else (mul-iter a (- b 1) (+ a add)))))

(fast-mul-iter 3 11)
```

# Exercise 1.20
Normal-order evaluation. The `remainder` function is evaluated 18 times as highlited below. Basically whenever we have to evaluate `gcd` function we replace with its body. But when evaluating if condition we only have inbuilt `remainder` function so we actually evaluate it.


- (gcd 206 40)

- (if (= (**remainder** 206 40) 
   40 
  (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
  
- (if (= ((**remainder** 40 ((**remainder** 206 40)) 0) 
  (remainder 206 40)
  (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
  
- (if (= ((**remainder** ((**remainder** 206 40) ((**remainder** 40 ((**remainder** 206 40))) 0) 
  (remainder 40 (remainder 206 40))
  (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
  
- (if (= ((**remainder** ((**remainder** 40 ((**remainder** 206 40)) ((**remainder** ((**remainder** 206 40) ((**remainder** 40 ((**remainder** 206 40))) 0)
  ((**remainder** ((**remainder** 206 40) ((**remainder** 40 ((**remainder** 206 40)))
  
Applicative-order evaluation.`remainder` is evaluated 4 times.
- (gcd 206 40)

- (if (= 40 0) 206 
  (gcd 40 (**remainder** 206 40))

- (gcd 40 6)  

- (if (= 6 0) 40 
  (gcd 6 (**remainder** 40 6))  

- (gcd 6 4)

- (if (= 4 0) 6 
  (gcd 4 (**remainder** 6 4))  

- (gcd 4 2)

- (if (= 2 0) 4 
  (gcd 2 (**remainder** 4 2))  
  
- (gcd 2 0)

- (if (= 0 0) 2 

# Exercise 1.21
```
(define (smallest-divisor n)
  (define (square b) (* b b))
  (define (find-divisor a)
    (cond ((> (square a) n) n)
          ((= (remainder n a) 0) a)
          (else (find-divisor (+ a 1)))))
  (find-divisor 2))
```
`(smallest-divisor 199)` **199**
`(smallest-divisor 1999)` **1999**
`(smallest-divisor 19999)` **7**

# Exercise 1.22
The square roots of 10K, 100K and 1000K have approximate ratio of 3 (3.16). So the runtime should also increase proportional to that. I tested but for me the ratio was coming out to be around 1.6. So I checked some other solutions and found that it doesn't hold up for small numbers as nowdays the processors are really fast. So I tested with 10^9, 10^10 and 10^11. For these numbers the ratio was around 3. Basically the runtimes are not constant and they keep changing for the same input, though not by a significant difference. So I just took a average (more like median) values for each input.
```
(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (next-odd n)
    (if (even? n)
        (+ n 1)
        (+ n 2)))
  (define (check n count)
    (cond ((> n end) count)
          ((prime? n) (check (next-odd n) (+ count 1)))
          (else (check (next-odd n) count))))  
  (check (if (even? start) (+ start 1) start) 0))

;(search-for-primes 1 10)

(define (three-primes start)
  (define (next-odd n)
    (if (even? n)
        (+ n 1)
        (+ n 2)))
  (define (check n count start-time)
    (cond ((= count 3) (report-prime (- (runtime) start-time)))
          ((prime? n) (check (next-odd n) (+ count 1) start-time))
          (else (check (next-odd n) count start-time))))  
  (check (if (even? start) (+ start 1) start) 0 (runtime)))
```
 
# Exercise 1.23
For small numbers given in the problem you don't see any/much difference. But for large numbers like 10^10 plus, the ratio is roughly 1.7-1.9 i.e. near 2.
```
(define (fast-smallest-divisor n)
  (define (square b) (* b b))
  (define (next b) (if (= b 2) 3 (+ b 2)))
  (define (find-divisor a)
    (cond ((> (square a) n) n)
          ((= (remainder n a) 0) a)
          (else (find-divisor (next a)))))
  (find-divisor 2))

(define (fast-prime? n)
  (= (fast-smallest-divisor n) n))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (three-primes start)
  (define (next-odd n)
    (if (even? n)
        (+ n 1)
        (+ n 2)))
  (define (check n count start-time)
    (cond ((= count 3) (report-prime (- (runtime) start-time)))
          ((fast-prime? n) (check (next-odd n) (+ count 1) start-time))
          (else (check (next-odd n) count start-time))))  
  (check (if (even? start) (+ start 1) start) 0 (runtime)))

(three-primes 1000000)
```
