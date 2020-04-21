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

# Exercise 3.4
```
(define (make-account2 balance original-pswd)
  (define invalid-attempts 0)
  (define (call-the-cops)
    (write "cops called!"))
  (define (withdraw amount)
    (if (> balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient balance"))
  (define (deposit amount)
    (if (< amount 0)
        "Amount should be > zero"
        (begin (set! balance (+ balance amount)) balance)))
  (define (handle-invalid-attempt)
    (if (eq? invalid-attempts 7)
        (call-the-cops)
        (begin (set! invalid-attempts (+ invalid-attempts 1)) (error "Invalid password"))))
  (define (dispatch type pswd)
    (cond ((not (eq? pswd original-pswd)) (handle-invalid-attempt))
          ((eq? type "w") (begin (set! invalid-attempts 0) withdraw))
          ((eq? type "d") (begin (set! invalid-attempts 0) deposit))          
          (else (error "unknown request"))))
  dispatch)

(define acc2 (make-account2 100 'secret))
((acc2 "w" 'secret) 50)
(acc2 "w" 'invalid)
```
# Exercise 3.7
```
(define (make-account2 balance original-pswd)
  (define invalid-attempts 0)
  (define (call-the-cops)
    (write "cops called!"))
  (define (withdraw amount)
    (if (> balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient balance"))
  (define (deposit amount)
    (if (< amount 0)
        "Amount should be > zero"
        (begin (set! balance (+ balance amount)) balance)))
  (define (handle-invalid-attempt)
    (if (eq? invalid-attempts 7)
        (call-the-cops)
        (begin (set! invalid-attempts (+ invalid-attempts 1)) (write "Invalid password"))))
  (define (dispatch type pswd)
    (cond ((not (eq? pswd original-pswd)) (handle-invalid-attempt))
          ((eq? type "w") (begin (set! invalid-attempts 0) withdraw))
          ((eq? type "d") (begin (set! invalid-attempts 0) deposit))
          ((eq? type "v") (eq? 1 1))
          ((eq? type "b") balance)
          (else (error "unknown request"))))
  dispatch)

(define acc2 (make-account2 100 'secret))
((acc2 "w" 'secret) 50)
(newline)

(define (make-joint account verify-password password)
  (if (account "v" verify-password)
      (lambda (type pswd)
        (if (eq? pswd password) (account type verify-password)
            "Invalid new password"))
      ("password verification failed")))

(define joint (make-joint acc2 'secret 'new))
(joint "b" 'new)
((acc2 "w" 'secret) 10)
(joint "b" 'new)
(acc2 "b" 'secret)
```
