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

# Exercise 3.2
```
(define (make-monitored f)
  (let ((counter 0))
    (lambda (input)
      (if (eq? 'how-many-calls? input)
          counter
          (begin (set! counter (+ counter 1)) (f input))))))

(define s (make-monitored sqrt))
(s 100)
(s 16)
(s 'how-many-calls?)
```
