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

# Exercise 3.3
```
(define (make-account2 balance original-pswd)
  (define (withdraw amount)
    (if (> balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient balance"))
  (define (deposit amount)
    (if (< amount 0)
        "Amount should be > zero"
        (begin (set! balance (+ balance amount)) balance)))
  (define (dispatch type pswd)
    (cond ((not (eq? pswd original-pswd)) (error "Invalid password"))
          ((eq? type "w") withdraw)
          ((eq? type "d") deposit)          
          (else (error "unknown request"))))
  dispatch)

(define acc2 (make-account2 100 'secret))
((acc2 "w" 'secret) 50)
((acc2 "w" 'invalid) 50)
```
