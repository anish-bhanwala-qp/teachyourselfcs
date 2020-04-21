# Exercise 3.1
```
(define (make-accumulator sum)
  (define (add s)
    (begin (set! sum (+ sum s)) sum))
  add)

(define A (make-accumulator 5))
(A 10)
(A 10)
```
